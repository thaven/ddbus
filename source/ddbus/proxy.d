module ddbus.proxy;

import alt.typecons;

import std.conv;
import std.string;
import std.traits;

import ddbus.thin : Connection, Message, DBusAny;
import ddbus.util : canDBus, allCanDBus;

/++
    UDA to set the DBus namespace of an interface
  +/
auto namespace(string nsName) {
    return NamespaceAttribute(nsName);
}

/++
    UDA to mark a function as signal binder

    A signal binder must have exactly one parameter, which must be a delegate
    type with a parameter list that matches the signal.
  +/
auto signal() @property {
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

  enum dbusNameOf = getUDAs!(I, NamespaceAttribute)[0].name;
}

class RemoteException : Exception {
  this(Proxy proxy, string method, Message msg)
  in { assert (msg.isError); }
  body {
    auto errorStr = "Exception thrown by service: " ~ errorName;

    if (msg.signature[0] == 's') {
      errorStr ~= ": " ~ msg.read!string();
    }

    super(errorStr);
    _msg = msg;
  }

  string errorName() @property const nothrow {
    return _msg.errorName;
  }

  /**
    Get the DBus error message represented by this exception.
  */
  const(Message) message() @property const nothrow @safe {
    return _msg;
  }

  private:
  Message _msg;
}

abstract class Proxy
{
  this(Connection conn, string dest, string path) {
    this._conn = conn;
    this._dest = dest.toStringz();
    this._path = path.toStringz();
  }

  protected:
  abstract const(char)* iface() const nothrow @property @safe;

  Ret _call(Ret, Args...)(string meth, Args args) if(allCanDBus!Args && canDBus!Ret) {
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
        auto args = msg.readTuple!(Parameters!DG);
        handler(args);
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

class DynamicProxy : Proxy
{
  this(Connection conn, string dest, string path, string iface) {
    super(conn, dest, path);
    _iface = iface.toStringz();
  }

  // TODO: use known method signature
  DBusAny opDispatch(string meth, Args...)(Args args) {
    auto ret = _call!Message(meth, args).readTuple!DBusAny();

    if (ret.tuple.length == 1)
      return ret.tuple[0];
    else
      return ret;
  }

  protected:
  override const(char)* iface() const nothrow @property @safe {
    return _iface;
  }

  private:
  const(char)* _iface;
}

I createProxy(I)(Connection conn, string dest, string path)
  if (isDBusInterface!I) {
  return new StaticProxy!I(conn, dest, path);
}

private: // --------------------------------------------------------------------

/+
abstract class StaticProxyBase : Proxy
{
  this(Connection conn, string dest, string path) {
    super(conn, dest, path);
  }

  protected:
  override const(char)* iface() const nothrow @property @safe {
    return dbusNameOf!I.ptr; // always a literal, thus \0 terminated
  }
}

alias StaticProxy(I) = AutoImplement!(I, StaticProxyBase, generateDBusProxy);
+/

class StaticProxy(I) : AutoImplement!(I, Proxy, generateDBusProxy)
{
  this(Connection conn, string dest, string path) {
    super(conn, dest, path);
  }

  protected:
  override const(char)* iface() const nothrow @property @safe {
    return dbusNameOf!I.ptr; // always a literal, thus \0 terminated
  }
}

struct NamespaceAttribute
{
    string name;
}

struct SignalAttribute { }

template generateDBusProxy(I, alias fun) {
  static if (isDBusMethod!fun)
    alias generateMethodProxy!(I, fun) generateDBusProxy;
  else static if (isDBusSignal!fun)
    alias generateSignalProxy!(I, fun) generateDBusProxy;
  else
    static assert (false);
}

template countOutParameters(alias fun, size_t i = Parameters!fun.length - 1) {
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

template countInParameters(alias fun, size_t i = 0) {
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

public {
string generateMethodProxy(I, alias fun)() @property {
  import std.traits;

  enum string meth = __traits(identifier, fun);

  alias ParameterIdentifierTuple!fun argNames;
  alias ParameterStorageClassTuple!fun argStors;
  alias ParameterStorageClass STC;

  static assert(argNames.length == argStors.length);

  string inArgsList;
  size_t outArgsCount;
  foreach (i, argStor; argStors) {
    static if (!(argStor & STC.out_)) {
      inArgsList ~= ", a" ~ i.to!string;

      enum bool isNonConstRef =
        (argStor & STC.ref_) && !is(Parameters!fun[i] == const);

//      static assert(!outArgsCount || isNonConstRef,
//        "DBus method must list output parameters after input parameters.");

      static if (!isNonConstRef)
        continue;
    }

//    ++outArgsCount;
  }

  alias ReturnType!fun R;
  enum haveReturn = !is(R == void);
  enum string argList = "(\"" ~ meth ~ "\", args[0 .. " ~ countInParameters!fun.to!string  ~ "])";

  if (!outArgsCount)
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

string generateSignalProxy(I, alias fun)() @property {
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

unittest {
  import ddbus.interfaces : DBus, RequestNameFlags, RequestNameReply;
  import ddbus.bus : BusService, BusPath;
  import ddbus.thin : connectToBus;

  DBus bus = createProxy!DBus(connectToBus(), BusService, BusPath);
  auto reply = bus.requestName("ca.thume.ddbus.testing",
    RequestNameFlags.doNotQueue | RequestNameFlags.allowReplacement);
  assert (reply == RequestNameReply.alreadyOwned
    || reply == RequestNameReply.primaryOwner);
}