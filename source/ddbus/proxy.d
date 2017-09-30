module ddbus.proxy;

import spec = ddbus.introspection;
import ddbus.thin : Connection, ObjectPath, Message, DBusAny;
import ddbus.util : canDBus, allCanDBus;

import alt.typecons;

import std.conv;
import std.exception : enforce;
import std.string;
import std.traits;

/++
    UDA to set the DBus namespace of an interface
  +/
auto namespace(string nsName)
{
    return NamespaceAttribute(nsName);
}

/++
    UDA to mark a function as signal binder

    A signal binder must have exactly one parameter, which must be a delegate
    type with a parameter list that matches the signal.
  +/
auto signal() @property
{
    return SignalAttribute.init;
}

template isDBusInterface(alias I)
  if (is(I == interface))
{
  alias hasUDA!(I, NamespaceAttribute) isDBusInterface;
}

template isDBusMethod(alias fun)
  if (__traits(isVirtualMethod, fun))
{
  enum isDBusMethod =
    isDBusInterface!(__traits(parent, fun)) && !hasUDA!(fun, SignalAttribute);
}

template isDBusSignal(alias fun)
  if (__traits(isVirtualMethod, fun))
{
  static if (isAnnotatedAsDBusSignal!fun) {
    import std.ascii : isUpper;

    private enum dName = __traits(identifier, fun);
    static if (dName[0..2] == "on" && isUpper(dName[2]) &&
      Parameters!fun.length == 1 && is(Parameters!fun[0] == delegate))
      enum isDBusSignal = true;
    else
    {
      pragma(msg, "Member "~dName~" is annotated as a DBus signal, but does not"
        ~ " meet the requirements for a signal binding method.");
      enum isDBusSignal = false;
    }
  }
  else
    enum isDBusSignal = false;

  private enum isAnnotatedAsDBusSignal(alias fun) =
    isDBusInterface!(__traits(parent, fun)) && hasUDA!(fun, SignalAttribute);
}

template dbusNameOf(alias I)
  if (isDBusInterface!I)
{
  static assert(getUDAs!(I, NamespaceAttribute).length == 1,
    "Only one @namespace attribute allowed.");

  enum dbusNameOf = getUDAs!(I, NamespaceAttribute)[0].name ~ '.' ~ I.stringof;
}

class RemoteException : Exception
{
  this(
    Proxy proxy,
    string method,
    Message msg)
  in
  {
    assert (msg.isError);
  }
  body
  {
    auto errorStr = "Exception thrown by service: " ~ errorName;

    if (msg.signature[0] == 's') {
      errorStr ~= ": " ~ msg.read!string();
    }

    super(errorStr);
    _msg = msg;
  }

  string errorName() @property const nothrow
  {
    return _msg.errorName;
  }

  /**
    Get the DBus error message represented by this exception.
  */
  const(Message) message() @property const nothrow @safe
  {
    return _msg;
  }

  private:
  Message _msg;
}

abstract class Proxy
{
  this(Connection conn, string dest, ObjectPath path)
  {
    this._conn = conn;
    this._dest = dest.toStringz();
    this._path = path.toString().toStringz();
  }

  protected:
  abstract const(char)* iface() const nothrow @property @safe;

  Ret _call(Ret, Args...)(string meth, Args args)
    if (allCanDBus!Args && (is(Ret == void) || canDBus!Ret))
  {
    import ddbus.c_lib : dbus_message_new_method_call;

    Message msg = Message(
      dbus_message_new_method_call(_dest, _path, iface, meth.toStringz()));
    msg.build(args);
    Message ret = this._conn.sendWithReplyBlocking(msg);

    if (ret.isError)
      throw new RemoteException(this, meth, ret);

    static if (!is(Ret == void)) {
      static if (is(Ret == Message))
        return ret;
      else
        return ret.read!Ret();
    }
  }

  void _bind(DG)(string sig, DG handler)
    if(is(DG == delegate) && allCanDBus!(Parameters!DG))
  {
    void handlerWrapper(Message msg) nothrow
    in { assert (msg.isSignal); }
    body {
      try {
        import std.typecons : Tuple;
        auto args = msg.readTuple!(Tuple!(Parameters!DG));
        handler(args.expand);
      } catch (Exception) {
        // FIXME:
        // Oops... what to do with the exception?
        // Should have some exception handling callback.
      }
    }

    _signalHandlers[sig] = &handlerWrapper;
  }

  private:
  Connection _conn;
  const(char)* _dest;
  const(char)* _path;
  void delegate(Message) nothrow[string] _signalHandlers;
}

/++
  Dynamic proxy

  To be used in case the interface of a remote object is not known at compile
  time.

  `DynamicProxy` utilizes introspection to obtain information about a remote
  object and uses that to proxy any calls, while checking argument types at
  runtime.
 +/
class DynamicProxy : Proxy
{
  /++
    Constructor

    Params:
      conn = The DBus connection to be used
      service = The name of the service that publishes the remote object
      path = Path to the object to be proxied
      iface = The name of the interface to be used
   +/
  this(
    Connection conn,
    string service,
    ObjectPath path,
    string iface)
  {
    import std.algorithm.iteration : filter;

    super(conn, service, path);

    auto ifaceSpecs = spec.introspect(conn, service, path)
      .interfaces
      .filter!(a => (a.name == iface));

    enforce(!ifaceSpecs.empty,
      "Object " ~ path.toString() ~ " does not support interface " ~ iface);

    _ifaceSpec = ifaceSpecs.front;
  }

  /++
    Call a remote method semi-statically

    This allows to use dot notation to make a call through a dynamic proxy.
    This is here because it may turn out to be handy sometimes, but generally
    a StaticProxy should be used in case the interface is known at compile time.
   +/
  Message opDispatch(string methodName, Args...)(Args args)
  {
    enforce(
      typeSigAll!Args == _ifaceSpec.getMethodByName(methodName).inputSignature,
      "Argument types don't match the signature obtained through introspection");

    return _call!Message(methodName, args);
  }

  /++
    Call a remote method dynamically

    Params:
      methodName = The name of the method to call.
      args = Either an array or an associative array holding any arguments to be
          passed to the remote method. If specified as an array, arguments are
          interpreted positionally, and their order must match the method
          specification exactly. If specified as an associative array, the
          keys match the argument names according to the method specification.
          Method arguments need to be wrapped in DBusAny because their types are
          unknown at compile time. Types of arguments are checked against the
          method specification obtained through introspection.

    Returns:
      A `Message` containing the value(s) returned by the remote method.

    Throws:
      `DBusException` if an error occurs while sending or receiving messages.
      `RemoteException` if the remote method returns an error.
   +/
  Message call(
    string methodName,
    DBusAny[] args)
  {
    auto methodSpec = &_ifaceSpec.getMethodByName(methodName);

    return _dynCall(methodSpec, (scope Message msg) {
      foreach (i, ref argument; methodSpec.arguments) {
        if (i >= args.length)
          break;

        if (argument.direction == spec.Argument.Direction.in_) {
          msg.build(args[i]);
        }
      }
    });
  }

  /// ditto
  Message call(
    string methodName,
    DBusAny[string] args)
  {
    auto methodSpec = &_ifaceSpec.getMethodByName(methodName);

    return _dynCall(methodSpec, (scope Message msg) {
      foreach (ref argument; methodSpec.arguments) {
        if (argument.direction == spec.Argument.Direction.in_) {
          msg.build(args[argument.name]);
        }
      }
    });
  }

  protected:
  override const(char)* iface() const nothrow @property @safe
  {
    return _ifaceSpec.name.toStringz();
  }

  private:
  immutable spec.Interface _ifaceSpec;

  Message _dynCall(
    const spec.Method * methodSpec,
    void delegate(scope Message) argBuilder)
  {
    import ddbus.c_lib : dbus_message_new_method_call;

    Message msg = Message(dbus_message_new_method_call(
        _dest, _path, iface, methodSpec.name.toStringz()));

    argBuilder(msg);

    enforce(
      msg.signature == methodSpec.inputSignature,
      "Argument types don't match the signature obtained through introspection");

    Message ret = this._conn.sendWithReplyBlocking(msg);

    if (ret.isError)
      throw new RemoteException(this, methodSpec.name, ret);

    return ret;
  }
}

I createProxy(I)(
  Connection conn,
  string service,
  ObjectPath path) if (isDBusInterface!I)
{
  return new StaticProxy!I(conn, service, path);
}

private: // --------------------------------------------------------------------

class StaticProxy(I) : AutoImplement!(I, Proxy, generateDBusProxy)
{
  this(
    Connection conn,
    string service,
    ObjectPath path)
  {
    super(conn, service, path);
  }

  protected:
  override const(char)* iface() const nothrow @property @safe
  {
    return &dbusNameOf!I[0]; // always a literal, thus \0 terminated
  }
}

struct NamespaceAttribute
{
    string name;
}

struct SignalAttribute { }

template generateDBusProxy(I, alias fun)
{
  static if (isDBusMethod!fun)
    alias generateMethodProxy!(I, fun) generateDBusProxy;
  else static if (isDBusSignal!fun)
    alias generateSignalProxy!(I, fun) generateDBusProxy;
  else
    static assert (false);
}

template countOutParameters(alias fun, size_t i = Parameters!fun.length - 1)
{
  static if (Parameters!fun.length == 0)
    enum countOutParameters = 0;
  else {
    private {
      enum argStor = ParameterStorageClassTuple!fun[i];
      enum isOut = cast(bool) (argStor & ParameterStorageClass.out_);
      enum isNonConstRef =
        (argStor & ParameterStorageClass.ref_)
        && !is(Parameters!fun[i] == const);
    }

    static if (isOut || isNonConstRef) {
      static if (i > 0)
        enum countOutParameters = 1 + countOutParameters!(fun, i - 1);
      else
        enum countOutParameters = 1;
    } else {
      enum countOutParameters = 0;
    }
  }
}

template countInParameters(alias fun, size_t i = 0)
{
  static if (Parameters!fun.length == 0)
    enum countInParameters = 0;
  else {
    private {
      enum argStor = ParameterStorageClassTuple!fun[i];
      enum isOut = cast(bool) (argStor & ParameterStorageClass.out_);
    }

    static if (!isOut) {
      static if (i < Parameters!fun.length - 1)
        enum countInParameters = 1 + countInParameters!(fun, i + 1);
      else
        enum countInParameters = 1;
    } else {
      enum countInParameters = 0;
    }
  }
}

string ucfirst(string s)
{
  import std.exception : assumeUnique;
  return assumeUnique([ cast(char) toUpper(s[0]) ]) ~ s[1 .. $];
}

public {
  // Functions in this block are public for technical reasons only.
  // They are NOT part of the API

  string generateMethodProxy(I, alias fun)() @property
  {

    import std.traits;

    // Identifier of method, with first letter uppercased
    enum string meth = ucfirst(__traits(identifier, fun));

    alias ParameterIdentifierTuple!fun argNames;

    alias ReturnType!fun R;
    enum haveReturn = !is(R == void);
    enum string argList = "(\"" ~ meth ~ "\", args[0 .. "
      ~ countInParameters!fun.to!string  ~ "])";

    if (!countOutParameters!fun)
      static if (haveReturn)
        return "return this._call!(typeof(return))" ~ argList ~ ';';
      else
        return "this._call!(void)" ~ argList ~ ';';
    else
    {
      auto code = "
        import ddbus.thin : Message;
        auto msg = this._call!(Message)" ~ argList ~ ";

        alias Tuple!(" ~ (haveReturn ? "typeof(return), " : "")
          ~ "Parameters!self[$ - " ~ countOutParameters!fun.to!string ~ " .. $]"
          ~ ") OutTuple;

        auto tup = msg.readTuple!OutTuple();";

      foreach (i, argName; argNames[$ - countOutParameters!fun .. $])
        code ~= argName ~ " = tup[" ~ (haveReturn ? i + 1 : i).to!string ~ "];";

      static if (haveReturn)
        code ~= "return tup[0];";

      return code;
    }
  }

  string generateSignalProxy(I, alias fun)() @property
  {

    import std.traits;

    alias ParameterStorageClassTuple!fun funArgStors;
    alias ParameterStorageClass STC;

    static assert(funArgStors.length == 1
      && !(funArgStors[0] & (STC.ref_ | STC.out_))
      && is(Parameters!fun[0] == delegate),
      "Signal binding method must have exactly one parameter, "
      ~ "which must be a delegate.");

    alias ParameterStorageClassTuple!(Parameters!fun[0]) argStors;

    foreach (i, argStor; argStors)
      static assert (!(argStor & (STC.out_ | STC.ref_)),
        "Signal handler cannot have out or ref parameters.");

    return "this._bind(\"" ~ __traits(identifier, fun)[2..$] ~ "\", a0);";
  }
}

unittest
{
  import ddbus.interfaces : DBus, RequestNameFlags, RequestNameReply;
  import ddbus.bus : BusService, BusPath;
  import ddbus.thin : connectToBus;

  DBus bus = createProxy!DBus(connectToBus(), BusService, ObjectPath(BusPath));
  auto reply = bus.requestName("ca.thume.ddbus.testing",
    RequestNameFlags.doNotQueue | RequestNameFlags.allowReplacement);
  assert (reply == RequestNameReply.alreadyOwned
    || reply == RequestNameReply.primaryOwner);
}
