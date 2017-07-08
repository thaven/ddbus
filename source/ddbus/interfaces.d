module ddbus.interfaces;

import ddbus.proxy : namespace, signal;
import ddbus.thin : Variant, DBusAny, ObjectPath;

@namespace("org.freedesktop") {
  enum RequestNameFlags : uint {
    none = 0,
    allowReplacement = 1,
    replaceExisting = 2,
    doNotQueue = 4
  }

  enum RequestNameReply : uint {
    primaryOwner = 1,
    inQueue = 2,
    exists = 3,
    alreadyOwned = 4
  }

  enum ReleaseNameReply : uint {
    released = 1,
    nonExistant = 2,
    notOwner = 3
  }

  enum StartServiceByNameReply : uint {
    success = 1,
    alreadyRunning = 2
  }

  interface DBus {
    string hello();
    RequestNameReply requestName(string name, RequestNameFlags flags);
    ReleaseNameReply releaseName(string name);
    StartServiceByNameReply startServiceByName(string name, uint flags);
    void updateActivationEnvironment(string[string] env_hash);
    bool nameHasOwner(string name);
    string[] listNames();
    string[] listActivatableNames();
    void addMatch(string rule);
    void removeMatch(string rule);
    string getNameOwner(string name);
    string[] listQueuedOwners(string name);
    uint getConnectionUnixUser(string name);
    uint getConnectionUnixProcessID(string name);
    byte[] getAdtAuditSessionData(string name);
    byte[] getConnectionSELinuxSecurityContext(string name);
    void reloadConfig();
    string getId();

    @signal void onNameOwnerChanged(
      void delegate(string name, string old_owner, string new_owner));
    @signal void onNameLost(void delegate(string name));
    @signal void onNameAcquired(void delegate(string name));
  }
}

@namespace("org.freedesktop.DBus") {
  interface Peer {
    void ping();
    string getMachineId();
  }

  interface Introspectable {
    string introspect();
  }

  interface Properties {
    Variant!DBusAny get(string interface_name, string property_name);
    void set(string interface_name, string property_name, Variant!DBusAny value);
    Variant!DBusAny[string] getAll(string interface_name);

    @signal void onPropertiesChanged(void delegate(string interface_name,
      Variant!DBusAny[string] changed_properties, string[] invalidated_properties));
  }

  interface ObjectManager {
    Variant!DBusAny[string][string][ObjectPath] getManagedObjects();

    @signal void onInterfacesAdded(void delegate(ObjectPath object_path,
      Variant!DBusAny[string][string] interfaces_and_properties));
    @signal void onInterfacesRemoved(
      void delegate(ObjectPath object_path, string[] interfaces));
  }
}
