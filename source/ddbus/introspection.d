module ddbus.introspection;

import ddbus.interfaces : Introspectable;
import ddbus.proxy : createProxy;
import ddbus.thin : Connection, ObjectPath;

import std.algorithm.iteration : each, filter, map;
import std.algorithm.searching : find;
import std.array: join;
import std.conv : to;
import std.exception : assumeUnique, enforce;
import std.experimental.xml : lexer, parser, cursor, domBuilder;
import std.range.primitives;
import std.typecons : Flag, No, Yes;

import dom = std.experimental.xml.dom;

private {
  alias DOMDocument = dom.Document!string;
  alias DOMElement = dom.Element!string;
}

struct Node
{
  private
  {
    string _name;
    Interface[] _interfaces;
  }

  private this(DOMElement elem) immutable
  in
  {
    assert(elem.tagName == "node");
  }
  body
  {
    _name = elem.getAttribute("name");

    if (elem.hasChildNodes) {
      immutable(Interface)[] interfaces;

      for (
        auto childNode = elem.firstChild;
        childNode !is null;
        childNode = childNode.nextSibling
      ) {
        enforce(childNode.nodeType == dom.NodeType.element);
        auto childElem = cast(DOMElement) childNode;

        enforce(childElem.tagName == "interface");
        interfaces ~= immutable(Interface)(childElem);
      }

      _interfaces = interfaces;
    }
  }

  string name() @property const pure nothrow @safe @nogc
  {
    return _name;
  }

  inout(Interface)[] interfaces() @property inout pure nothrow @safe @nogc
  {
    return _interfaces;
  }

  string toXMLString() const
  {
    import std.experimental.xml.domimpl : DOMImplementation;

    auto impl = new DOMImplementation!string;
    auto doctype = impl.createDocumentType("node",
      "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN",
      "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd"
    );

    auto doc = impl.createDocument(null, "node", doctype);
    auto elem = doc.documentElement;

    elem.setAttribute("name", name);
    interfaces.each!(iface => elem.appendChild(iface.toXML(doc)));

    import std.array : Appender;
    import std.experimental.xml.writer;

    auto appender = Appender!string();
    appender.writerFor!string.writeDOM(doc);

    return appender.data;
  }

  private DOMElement toXML(DOMDocument doc) const
  {
    auto elem = doc.createElement("node");

    elem.setAttribute("name", name);
    interfaces.each!(iface => elem.appendChild(iface.toXML(doc)));

    return elem;
  }
}

struct Interface
{
  private
  {
    string _name;
    Method[] _methods;
    Signal[] _signals;
    Property[] _properties;
    Annotation[] _annotations;
  }

  private this(DOMElement elem) immutable
  in
  {
    assert(elem.tagName == "interface");
  }
  body
  {
    _name = elem.getAttribute("name");

    if (elem.hasChildNodes) {
      immutable(Method)[] methods;
      immutable(Signal)[] signals;
      immutable(Property)[] properties;
      immutable(Annotation)[] annotations;

      for (
        auto childNode = elem.firstChild;
        childNode !is null;
        childNode = childNode.nextSibling
      ) {
        enforce(childNode.nodeType == dom.NodeType.element);
        auto childElem = cast(DOMElement) childNode;

        switch (childElem.tagName) {
          case "method":
            methods ~= immutable(Method)(childElem);
            break;
          case "signal":
            signals ~= immutable(Signal)(childElem);
            break;
          case "property":
            properties ~= immutable(Property)(childElem);
            break;
          case "annotation":
            annotations ~= immutable(Annotation)(childElem);
            break;
          default:
            throw new Exception("Unexpected element: " ~ childElem.tagName);
        }
      }

      _methods = methods;
      _signals = signals;
      _properties = properties;
      _annotations = annotations;
    }
  }

  string name() @property const pure nothrow @safe @nogc
  {
    return _name;
  }

  const(Method)* findMethodByName(string name) const nothrow @trusted @nogc
  {
    auto result = _methods.find!(a => a.name == name);
    return !result.empty ? &result.front : null;
  }

  ref const(Method) getMethodByName(string name) const @safe
  {
    auto method = findMethodByName(name);
    enforce(method !is null, "Method not found: " ~ name);
    return *method;
  }

  const(Property)* findPropertyByName(string name) const nothrow @trusted @nogc
  {
    auto result = _properties.find!(a => a.name == name);
    return !result.empty ? &result.front : null;
  }

  ref const(Property) getPropertyByName(string name) const @safe
  {
    auto property = findPropertyByName(name);
    enforce(property !is null, "Property not found: " ~ name);
    return *property;
  }

  const(Signal)* findSignalByName(string name) const nothrow @trusted @nogc
  {
    auto result = _signals.find!(a => a.name == name);
    return !result.empty ? &result.front : null;
  }

  ref const(Signal) getSignalByName(string name) const @safe
  {
    auto signal = findSignalByName(name);
    enforce(signal !is null, "Signal not found: " ~ name);
    return *signal;
  }

  private DOMElement toXML(DOMDocument doc) const
  {
    auto elem = doc.createElement("interface");

    elem.setAttribute("name", name);
    _annotations.each!(annotation => elem.appendChild(annotation.toXML(doc)));
    _methods.each!(method => elem.appendChild(method.toXML(doc)));
    _signals.each!(signal => elem.appendChild(signal.toXML(doc)));
    _properties.each!(property => elem.appendChild(property.toXML(doc)));

    return elem;
  }
}

struct Method {
  private {
    string _name;
    Argument[] _arguments;
    Annotation[] _annotations;
  }

  private this(DOMElement elem) immutable
  in
  {
    assert(elem.tagName == "method");
  }
  body
  {
    _name = elem.getAttribute("name");

    if (elem.hasChildNodes) {
      immutable(Argument)[] arguments;
      immutable(Annotation)[] annotations;

      for (
        auto childNode = elem.firstChild;
        childNode !is null;
        childNode = childNode.nextSibling
      ) {
        enforce(childNode.nodeType == dom.NodeType.element);
        auto childElem = cast(DOMElement) childNode;

        switch (childElem.tagName) {
          case "arg":
            arguments ~= immutable(Argument)(childElem);
            break;
          case "annotation":
            annotations ~= immutable(Annotation)(childElem);
            break;
          default:
            throw new Exception("Unexpected element: " ~ childElem.tagName);
        }
      }

      _arguments = arguments;
      _annotations = annotations;
    }
  }

  string name() @property const pure nothrow @safe @nogc
  {
    return _name;
  }

  inout(Argument[]) arguments() @property inout pure nothrow @safe @nogc
  {
    return _arguments;
  }

  string inputSignature() @property const nothrow @safe
  {
    return _arguments
      .filter!(arg => arg.direction == Argument.Direction.in_)
      .map!(arg => arg.typeSignature)
      .join();
  }

  string outputSignature() @property const nothrow @safe
  {
    return _arguments
      .filter!(arg => arg.direction == Argument.Direction.out_)
      .map!(arg => arg.typeSignature)
      .join();
  }

  private DOMElement toXML(DOMDocument doc) const
  {
    auto elem = doc.createElement("method");

    elem.setAttribute("name", name);
    _annotations.each!(annotation => elem.appendChild(annotation.toXML(doc)));
    _arguments.each!(argument => elem.appendChild(argument.toXML(doc)));

    return elem;
  }
}

struct Signal
{
  private
  {
    string _name;
    Argument[] _arguments;
    Annotation[] _annotations;
  }

  private this(DOMElement elem) immutable
  in
  {
    assert(elem.tagName == "signal");
  }
  body
  {
    _name = elem.getAttribute("name");

    if (elem.hasChildNodes) {
      immutable(Argument)[] arguments;
      immutable(Annotation)[] annotations;

      for (
        auto childNode = elem.firstChild;
        childNode !is null;
        childNode = childNode.nextSibling
      ) {
        enforce(childNode.nodeType == dom.NodeType.element);
        auto childElem = cast(DOMElement) childNode;

        switch (childElem.tagName) {
          case "arg":
            auto argument = immutable(Argument)(childElem);
            enforce(argument.direction == Argument.Direction.in_,
              "A signal cannot have arguments with direction 'out'.");
            arguments ~= argument;
            break;
          case "annotation":
            annotations ~= immutable(Annotation)(childElem);
            break;
          default:
            throw new Exception("Unexpected element: " ~ childElem.tagName);
        }
      }

      _arguments = arguments;
      _annotations = annotations;
    }
  }

  string name() @property const pure nothrow @safe @nogc
  {
    return _name;
  }

  string inputSignature() @property const nothrow @safe
  {
    return _arguments.map!(arg => arg.typeSignature).join();
  }

  private DOMElement toXML(DOMDocument doc) const
  {
    auto elem = doc.createElement("signal");

    elem.setAttribute("name", name);
    _annotations.each!(annotation => elem.appendChild(annotation.toXML(doc)));
    _arguments.each!(argument => elem.appendChild(argument.toXML(doc)));

    return elem;
  }
}

struct Argument
{
  enum Direction { in_, out_ }

  private
  {
    string _name;
    string _typeSignature;
    Direction _direction;
  }

  private this(DOMElement elem) immutable
  in
  {
    assert(elem.tagName == "arg");
  }
  body
  {
    _name = elem.getAttribute("name");
    _typeSignature = elem.getAttribute("type");

    Direction direction;

    switch (elem.getAttribute("direction")) {
      case null:
      case "in":
        direction = Direction.in_;
        break;
      case "out":
        direction = Direction.out_;
        break;
      default:
        throw new Exception("Invalid value '" ~ elem.getAttribute("direction")
          ~ "' of attribute 'direction' for argument '" ~ name ~ "'");
    }

    _direction = direction;

    enforce(!elem.hasChildNodes, "Tag <arg> should not have any contents.");
  }

  string name() @property const pure nothrow @safe @nogc
  {
    return _name;
  }

  string typeSignature() @property const pure nothrow @safe @nogc
  {
    return _typeSignature;
  }

  Direction direction() @property const pure nothrow @safe @nogc
  {
    return _direction;
  }

  private DOMElement toXML(DOMDocument doc) const
  {
    import std.algorithm.mutation : stripRight;

    auto elem = doc.createElement("argument");

    elem.setAttribute("name", name);
    elem.setAttribute("type", typeSignature);
    elem.setAttribute("direction", direction.to!string.stripRight('_'));

    return elem;
  }
}

struct Property
{
  enum Access { read, write, readwrite }

  private
  {
    string _name;
    string _typeSignature;
    Access _access;
    Annotation[] _annotations;
  }

  private this(DOMElement elem) immutable
  in
  {
    assert(elem.tagName == "property");
  }
  body
  {
    _name = elem.getAttribute("name");
    _typeSignature = elem.getAttribute("type");

    Access access;

    switch (elem.getAttribute("access")) {
      case "read":
        access = Access.read;
        break;
      case "write":
        access = Access.write;
        break;
      case "readwrite":
        access = Access.readwrite;
        break;
      default:
        throw new Exception("Invalid value '" ~ elem.getAttribute("access")
          ~ "' of attribute 'access' for property '" ~ name ~ "'");
    }

    access = access;

    if (elem.hasChildNodes) {
      immutable(Annotation)[] annotations;

      for (
        auto childNode = elem.firstChild;
        childNode !is null;
        childNode = childNode.nextSibling
      ) {
        enforce(childNode.nodeType == dom.NodeType.element);
        auto childElem = cast(DOMElement) childNode;

        enforce(childElem.tagName == "annotation");
        annotations ~= immutable(Annotation)(childElem);
      }

      _annotations = annotations;
    }
  }

  string name() @property const pure nothrow @safe @nogc
  {
    return _name;
  }

  string typeSignature() @property const pure nothrow @safe @nogc
  {
    return _typeSignature;
  }

  Access access() @property const pure nothrow @safe @nogc
  {
    return _access;
  }

  private DOMElement toXML(DOMDocument doc) const
  {
    auto elem = doc.createElement("property");

    elem.setAttribute("name", name);
    elem.setAttribute("type", typeSignature);
    elem.setAttribute("access", access.to!string);
    _annotations.each!(annotation => elem.appendChild(annotation.toXML(doc)));

    return elem;
  }
}

struct Annotation
{
  private
  {
    string _name;
    string _value;
  }

  private this(DOMElement elem) immutable
  in
  {
    assert(elem.tagName == "annotation");
  }
  body
  {
    _name = elem.getAttribute("name");
    _value = elem.getAttribute("value");

    enforce(!elem.hasChildNodes, "Tag <annotation> should not have any contents.");
  }

  string name() @property const pure nothrow @safe @nogc
  {
    return _name;
  }

  string value() @property const pure nothrow @safe @nogc
  {
    return _value;
  }

  private DOMElement toXML(DOMDocument doc) const
  {
    auto elem = doc.createElement("annotation");

    elem.setAttribute("name", name);
    elem.setAttribute("value", value);

    return elem;
  }
}

/++
    Get and parse introspection information from a remote object

    Params:
      conn = The DBus connection to be used
      service = The name of the service that publishes the remote object
      path = Path to the object to introspect

    Returns:
      A `Node` object describing the remote object.

    Throws:
      DBusException in case of DBus related errors.
      RemoteException in case the remote object does not support introspection
          or encounters an exception while building its introspection XML.
      XML parsing and validation related exceptions in the unlikely case that
          invalid introspection XML was received.
 +/
ref immutable(Node) introspect(
  Connection conn,
  string service,
  ObjectPath path)
{
  auto obj = createProxy!Introspectable(conn, service, path);
  string xml = obj.introspect();
  auto docElem = xml.lexer.parser.cursor.domBuilder.getDocument.documentElement;

  enforce(
    docElem.tagName == "node"
    && docElem.getAttribute("name") == path.toString()
  );

  return *(new immutable(Node)(docElem));
}

