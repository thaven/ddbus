// Written in the D programming language.

/// This module provides an extended implementation of Phobos'
/// std.typecons.AutoImplement and friends.
///
/// The patch is also published as PR #5408 to Phobos
module alt.typecons;

import std.traits;
import std.meta;

/**
$(D BlackHole!Base) is a subclass of $(D Base) which automatically implements
all abstract member functions in $(D Base) as do-nothing functions.  Each
auto-implemented function just returns the default value of the return type
without doing anything.

The name came from
$(HTTP search.cpan.org/~sburke/Class-_BlackHole-0.04/lib/Class/_BlackHole.pm, Class::_BlackHole)
Perl module by Sean M. Burke.

Params:
    Base = A non-final class for `BlackHole` to inherit from.

See_Also:
  $(LREF AutoImplement), $(LREF generateEmptyFunction)
 */
alias BlackHole(Base) = AutoImplement!(Base, generateEmptyFunction, isAbstractFunction);

///
@system unittest
{
    import std.math : isNaN;

    static abstract class C
    {
        int m_value;
        this(int v) { m_value = v; }
        int value() @property { return m_value; }

        abstract real realValue() @property;
        abstract void doSomething();
    }

    auto c = new BlackHole!C(42);
    assert(c.value == 42);

    // Returns real.init which is NaN
    assert(c.realValue.isNaN);
    // Abstract functions are implemented as do-nothing
    c.doSomething();
}

@system unittest
{
    import std.math : isNaN;

    // return default
    {
        interface I_1 { real test(); }
        auto o = new BlackHole!I_1;
        assert(o.test().isNaN()); // NaN
    }
    // doc example
    {
        static class C
        {
            int m_value;
            this(int v) { m_value = v; }
            int value() @property { return m_value; }

            abstract real realValue() @property;
            abstract void doSomething();
        }

        auto c = new BlackHole!C(42);
        assert(c.value == 42);

        assert(c.realValue.isNaN); // NaN
        c.doSomething();
    }

    // Bugzilla 12058
    interface Foo
    {
        inout(Object) foo() inout;
    }
    BlackHole!Foo o;
}


/**
$(D WhiteHole!Base) is a subclass of $(D Base) which automatically implements
all abstract member functions as functions that always fail. These functions
simply throw an $(D Error) and never return. `Whitehole` is useful for
trapping the use of class member functions that haven't been implemented.

The name came from
$(HTTP search.cpan.org/~mschwern/Class-_WhiteHole-0.04/lib/Class/_WhiteHole.pm, Class::_WhiteHole)
Perl module by Michael G Schwern.

Params:
    Base = A non-final class for `WhiteHole` to inherit from.

See_Also:
  $(LREF AutoImplement), $(LREF generateAssertTrap)
 */
alias WhiteHole(Base) = AutoImplement!(Base, generateAssertTrap, isAbstractFunction);

///
@system unittest
{
    import std.exception : assertThrown;

    static class C
    {
        abstract void notYetImplemented();
    }

    auto c = new WhiteHole!C;
    assertThrown!NotImplementedError(c.notYetImplemented()); // throws an Error
}

// / ditto
class NotImplementedError : Error
{
    this(string method)
    {
        super(method ~ " is not implemented");
    }
}

@system unittest
{
    import std.exception : assertThrown;
    // nothrow
    {
        interface I_1
        {
            void foo();
            void bar() nothrow;
        }
        auto o = new WhiteHole!I_1;
        assertThrown!NotImplementedError(o.foo());
        assertThrown!NotImplementedError(o.bar());
    }
    // doc example
    {
        static class C
        {
            abstract void notYetImplemented();
        }

        auto c = new WhiteHole!C;
        try
        {
            c.notYetImplemented();
            assert(0);
        }
        catch (Error e) {}
    }
}


/**
$(D AutoImplement) automatically implements (by default) all abstract member
functions in the class or interface $(D Base) in specified way.

The second version of $(D AutoImplement) automatically implements
$(D Interface), while deriving from $(D BaseClass).

Params:
  how  = template which specifies _how functions will be implemented/overridden.

         Two arguments are passed to $(D how): the type $(D Base) and an alias
         to an implemented function.  Then $(D how) must return an implemented
         function body as a string.

         The generated function body can use these keywords:
         $(UL
            $(LI $(D a0), $(D a1), &hellip;: arguments passed to the function;)
            $(LI $(D args): a tuple of the arguments;)
            $(LI $(D self): an alias to the function itself;)
            $(LI $(D parent): an alias to the overridden function (if any).)
         )

        You may want to use templated property functions (instead of Implicit
        Template Properties) to generate complex functions:
--------------------
// Prints log messages for each call to overridden functions.
string generateLogger(C, alias fun)() @property
{
    import std.traits;
    enum qname = C.stringof ~ "." ~ __traits(identifier, fun);
    string stmt;

    stmt ~= q{ struct Importer { import std.stdio; } };
    stmt ~= `Importer.writeln("Log: ` ~ qname ~ `(", args, ")");`;
    static if (!__traits(isAbstractFunction, fun))
    {
        static if (is(ReturnType!fun == void))
            stmt ~= q{ parent(args); };
        else
            stmt ~= q{
                auto r = parent(args);
                Importer.writeln("--> ", r);
                return r;
            };
    }
    return stmt;
}
--------------------

  what = template which determines _what functions should be
         implemented/overridden.

         An argument is passed to $(D what): an alias to a non-final member
         function in $(D Base).  Then $(D what) must return a boolean value.
         Return $(D true) to indicate that the passed function should be
         implemented/overridden.

--------------------
// Sees if fun returns something.
enum bool hasValue(alias fun) = !is(ReturnType!(fun) == void);
--------------------


Note:

Generated code is inserted in the scope of $(D std.typecons) module.  Thus,
any useful functions outside $(D std.typecons) cannot be used in the generated
code.  To workaround this problem, you may $(D import) necessary things in a
local struct, as done in the $(D generateLogger()) template in the above
example.


BUGS:

$(UL
 $(LI Variadic arguments to constructors are not forwarded to super.)
 $(LI Deep interface inheritance causes compile error with messages like
      "Error: function std.typecons._AutoImplement!(Foo)._AutoImplement.bar
      does not override any function".  [$(BUGZILLA 2525), $(BUGZILLA 3525)] )
 $(LI The $(D parent) keyword is actually a delegate to the super class'
      corresponding member function.  [$(BUGZILLA 2540)] )
 $(LI Using alias template parameter in $(D how) and/or $(D what) may cause
     strange compile error.  Use template tuple parameter instead to workaround
     this problem.  [$(BUGZILLA 4217)] )
)
 */
class AutoImplement(Base, alias how, alias what = isAbstractFunction) : Base
    if (!is(how == class))
{
    private alias autoImplement_helper_ =
        AutoImplement_Helper!("autoImplement_helper_", "Base", Base, typeof(this), how, what);
    mixin(autoImplement_helper_.code);
}

/// ditto
class AutoImplement(
    Interface, BaseClass, alias how,
    alias what = isAbstractFunction) : BaseClass, Interface
    if (is(Interface == interface) && is(BaseClass == class))
{
    private alias autoImplement_helper_ = AutoImplement_Helper!(
            "autoImplement_helper_", "Interface", Interface, typeof(this), how, what);
    mixin(autoImplement_helper_.code);
}

/*
 * Code-generating stuffs are encupsulated in this helper template so that
 * namespace pollution, which can cause name confliction with Base's public
 * members, should be minimized.
 */
private template AutoImplement_Helper(string myName, string baseName,
        Base, Self, alias generateMethodBody, alias cherrypickMethod)
{
private static:
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
    // Internal stuffs
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

    // Returns function overload sets in the class C, filtered with pred.
    template enumerateOverloads(C, alias pred)
    {
        template Impl(names...)
        {
            import std.meta : Filter;
            static if (names.length > 0)
            {
                alias methods = Filter!(pred, MemberFunctionsTuple!(C, names[0]));
                alias next = Impl!(names[1 .. $]);

                static if (methods.length > 0)
                    alias Impl = AliasSeq!(OverloadSet!(names[0], methods), next);
                else
                    alias Impl = next;
            }
            else
                alias Impl = AliasSeq!();
        }

        alias enumerateOverloads = Impl!(__traits(allMembers, C));
    }

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
    // Target functions
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

    // Add a non-final check to the cherrypickMethod.
    enum bool canonicalPicker(fun.../+[BUG 4217]+/) =
        !__traits(isFinalFunction, fun[0]) && cherrypickMethod!(fun);

    /*
     * A tuple of overload sets, each item of which consists of functions to be
     * implemented by the generated code.
     */
    alias targetOverloadSets = enumerateOverloads!(Base, canonicalPicker);

    /*
     * Super class of this AutoImplement instance
     */
    alias Super = BaseTypeTuple!(Self)[0];
    static assert(is(Super == class));
    static assert(is(Base == interface) || is(Super == Base));

    /*
     * A tuple of the super class' constructors.  Used for forwarding
     * constructor calls.
     */
    static if (__traits(hasMember, Super, "__ctor"))
        alias ctorOverloadSet = OverloadSet!("__ctor", __traits(getOverloads, Super, "__ctor"));
    else
        alias ctorOverloadSet = OverloadSet!("__ctor"); // empty


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
    // Type information
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

    /*
     * The generated code will be mixed into AutoImplement, which will be
     * instantiated in this module's scope.  Thus, any user-defined types are
     * out of scope and cannot be used directly (i.e. by their names).
     *
     * We will use FuncInfo instances for accessing return types and parameter
     * types of the implemented functions.  The instances will be populated to
     * the AutoImplement's scope in a certain way; see the populate() below.
     */

    // Returns the preferred identifier for the FuncInfo instance for the i-th
    // overloaded function with the name.
    template INTERNAL_FUNCINFO_ID(string name, size_t i)
    {
        import std.format : format;

        enum string INTERNAL_FUNCINFO_ID = format("F_%s_%s", name, i);
    }

    /*
     * Insert FuncInfo instances about all the target functions here.  This
     * enables the generated code to access type information via, for example,
     * "autoImplement_helper_.F_foo_1".
     */
    template populate(overloads...)
    {
        static if (overloads.length > 0)
        {
            mixin populate!(overloads[0].name, overloads[0].contents);
            mixin populate!(overloads[1 .. $]);
        }
    }
    template populate(string name, methods...)
    {
        static if (methods.length > 0)
        {
            mixin populate!(name, methods[0 .. $ - 1]);
            //
            alias target = methods[$ - 1];
            enum ith = methods.length - 1;
            mixin("alias " ~ INTERNAL_FUNCINFO_ID!(name, ith) ~ " = FuncInfo!target;");
        }
    }

    public mixin populate!(targetOverloadSets);
    public mixin populate!(  ctorOverloadSet );


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
    // Code-generating policies
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

    /* Common policy configurations for generating constructors and methods. */
    template CommonGeneratingPolicy()
    {
        // base class identifier which generated code should use
        enum string BASE_CLASS_ID = baseName;

        // FuncInfo instance identifier which generated code should use
        template FUNCINFO_ID(string name, size_t i)
        {
            enum string FUNCINFO_ID =
                myName ~ "." ~ INTERNAL_FUNCINFO_ID!(name, i);
        }
    }

    /* Policy configurations for generating constructors. */
    template ConstructorGeneratingPolicy()
    {
        mixin CommonGeneratingPolicy;

        /* Generates constructor body.  Just forward to the base class' one. */
        string generateFunctionBody(ctor.../+[BUG 4217]+/)() @property
        {
            enum varstyle = variadicFunctionStyle!(typeof(&ctor[0]));

            static if (varstyle & (Variadic.c | Variadic.d))
            {
                // the argptr-forwarding problem
                //pragma(msg, "Warning: AutoImplement!(", Base, ") ",
                //        "ignored variadic arguments to the constructor ",
                //        FunctionTypeOf!(typeof(&ctor[0])) );
            }
            return "super(args);";
        }
    }

    /* Policy configurations for genearting target methods. */
    template MethodGeneratingPolicy()
    {
        mixin CommonGeneratingPolicy;

        /* Geneartes method body. */
        string generateFunctionBody(func.../+[BUG 4217]+/)() @property
        {
            return generateMethodBody!(Base, func); // given
        }
    }


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
    // Generated code
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

    alias ConstructorGenerator = MemberFunctionGenerator!(ConstructorGeneratingPolicy!());
    alias MethodGenerator      = MemberFunctionGenerator!(MethodGeneratingPolicy!());

    public enum string code =
        ConstructorGenerator.generateCode!(  ctorOverloadSet ) ~ "\n" ~
             MethodGenerator.generateCode!(targetOverloadSets);

    debug (SHOW_GENERATED_CODE)
    {
        pragma(msg, "-------------------- < ", Base, " >");
        pragma(msg, code);
        pragma(msg, "--------------------");
    }
}

//debug = SHOW_GENERATED_CODE;
@system unittest
{
    import core.vararg;
    // no function to implement
    {
        interface I_1 {}
        auto o = new BlackHole!I_1;
    }
    // parameters
    {
        interface I_3 { void test(int, in int, out int, ref int, lazy int); }
        auto o = new BlackHole!I_3;
    }
    // use of user-defined type
    {
        struct S {}
        interface I_4 { S test(); }
        auto o = new BlackHole!I_4;
    }
    // overloads
    {
        interface I_5
        {
            void test(string);
            real test(real);
            int  test();
        }
        auto o = new BlackHole!I_5;
    }
    // constructor forwarding
    {
        static class C_6
        {
            this(int n) { assert(n == 42); }
            this(string s) { assert(s == "Deeee"); }
            this(...) {}
        }
        auto o1 = new BlackHole!C_6(42);
        auto o2 = new BlackHole!C_6("Deeee");
        auto o3 = new BlackHole!C_6(1, 2, 3, 4);
    }
    // attributes
    {
        interface I_7
        {
            ref int test_ref();
            int test_pure() pure;
            int test_nothrow() nothrow;
            int test_property() @property;
            int test_safe() @safe;
            int test_trusted() @trusted;
            int test_system() @system;
            int test_pure_nothrow() pure nothrow;
        }
        auto o = new BlackHole!I_7;
    }
    // storage classes
    {
        interface I_8
        {
            void test_const() const;
            void test_immutable() immutable;
            void test_shared() shared;
            void test_shared_const() shared const;
        }
        auto o = new BlackHole!I_8;
    }
    // use baseclass
    {
        static class C_9
        {
            private string foo_;

            this(string s) {
                foo_ = s;
            }

            protected string boilerplate() @property
            {
                return "Boilerplate stuff.";
            }

            public string foo() @property
            {
                return foo_;
            }
        }

        interface I_10
        {
            string testMethod(size_t);
        }

        static string generateTestMethod(C, alias fun)() @property
        {
            return "return this.boilerplate[0 .. a0];";
        }

        auto o = new AutoImplement!(I_10, C_9, generateTestMethod)("Testing");
        assert(o.testMethod(11) == "Boilerplate");
        assert(o.foo == "Testing");
    }
    /+ // deep inheritance
    {
    // XXX [BUG 2525,3525]
    // NOTE: [r494] func.c(504-571) FuncDeclaration::semantic()
        interface I { void foo(); }
        interface J : I {}
        interface K : J {}
        static abstract class C_9 : K {}
        auto o = new BlackHole!C_9;
    }+/
}

// Issue 17177 - AutoImplement fails on function overload sets with "cannot infer type from overloaded function symbol"
@system unittest
{
    static class Issue17177
    {
        private string n_;

        public {
            Issue17177 overloaded(string n)
            {
                this.n_ = n;

                return this;
            }

            string overloaded()
            {
                return this.n_;
            }
        }
    }

    static string how(C, alias fun)()
    {
        static if (!is(ReturnType!fun == void))
        {
            return q{
                return parent(args);
            };
        }
        else
        {
            return q{
                parent(args);
            };
        }
    }

    alias Implementation = AutoImplement!(Issue17177, how, templateNot!isFinalFunction);
}

version(unittest)
{
    // Issue 10647
    // Add prefix "issue10647_" as a workaround for issue 1238
    private string issue10647_generateDoNothing(C, alias fun)() @property
    {
        string stmt;

        static if (is(ReturnType!fun == void))
            stmt ~= "";
        else
        {
            string returnType = ReturnType!fun.stringof;
            stmt ~= "return "~returnType~".init;";
        }
        return stmt;
    }

    private template issue10647_isAlwaysTrue(alias fun)
    {
        enum issue10647_isAlwaysTrue = true;
    }

    // Do nothing template
    private template issue10647_DoNothing(Base)
    {
        alias issue10647_DoNothing = AutoImplement!(Base, issue10647_generateDoNothing, issue10647_isAlwaysTrue);
    }

    // A class to be overridden
    private class issue10647_Foo{
        void bar(int a) { }
    }
}
@system unittest
{
    auto foo = new issue10647_DoNothing!issue10647_Foo();
    foo.bar(13);
}

/*
Used by MemberFunctionGenerator.
 */
package template OverloadSet(string nam, T...)
{
    enum string name = nam;
    alias contents = T;
}

/*
Used by MemberFunctionGenerator.
 */
package template FuncInfo(alias func, /+[BUG 4217 ?]+/ T = typeof(&func))
{
    alias RT = ReturnType!T;
    alias PT = Parameters!T;
}
package template FuncInfo(Func)
{
    alias RT = ReturnType!Func;
    alias PT = Parameters!Func;
}

/*
General-purpose member function generator.
--------------------
template GeneratingPolicy()
{
    // [optional] the name of the class where functions are derived
    enum string BASE_CLASS_ID;

    // [optional] define this if you have only function types
    enum bool WITHOUT_SYMBOL;

    // [optional] Returns preferred identifier for i-th parameter.
    template PARAMETER_VARIABLE_ID(size_t i);

    // Returns the identifier of the FuncInfo instance for the i-th overload
    // of the specified name.  The identifier must be accessible in the scope
    // where generated code is mixed.
    template FUNCINFO_ID(string name, size_t i);

    // Returns implemented function body as a string.  When WITHOUT_SYMBOL is
    // defined, the latter is used.
    template generateFunctionBody(alias func);
    template generateFunctionBody(string name, FuncType);
}
--------------------
 */
package template MemberFunctionGenerator(alias Policy)
{
private static:
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
    // Internal stuffs
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
    import std.format;

    enum CONSTRUCTOR_NAME = "__ctor";

    // true if functions are derived from a base class
    enum WITH_BASE_CLASS = __traits(hasMember, Policy, "BASE_CLASS_ID");

    // true if functions are specified as types, not symbols
    enum WITHOUT_SYMBOL = __traits(hasMember, Policy, "WITHOUT_SYMBOL");

    // preferred identifier for i-th parameter variable
    static if (__traits(hasMember, Policy, "PARAMETER_VARIABLE_ID"))
    {
        alias PARAMETER_VARIABLE_ID = Policy.PARAMETER_VARIABLE_ID;
    }
    else
    {
        enum string PARAMETER_VARIABLE_ID(size_t i) = format("a%s", i);
            // default: a0, a1, ...
    }

    // Returns a tuple consisting of 0,1,2,...,n-1.  For static foreach.
    template CountUp(size_t n)
    {
        static if (n > 0)
            alias CountUp = AliasSeq!(CountUp!(n - 1), n - 1);
        else
            alias CountUp = AliasSeq!();
    }


    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://
    // Code generator
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::://

    /*
     * Runs through all the target overload sets and generates D code which
     * implements all the functions in the overload sets.
     */
    public string generateCode(overloads...)() @property
    {
        string code = "";

        // run through all the overload sets
        foreach (i_; CountUp!(0 + overloads.length)) // workaround
        {
            enum i = 0 + i_; // workaround
            alias oset = overloads[i];

            code ~= generateCodeForOverloadSet!(oset);

            static if (WITH_BASE_CLASS && oset.name != CONSTRUCTOR_NAME)
            {
                // The generated function declarations may hide existing ones
                // in the base class (cf. HiddenFuncError), so we put an alias
                // declaration here to reveal possible hidden functions.
                code ~= format("alias %s = %s.%s;\n",
                            oset.name,
                            Policy.BASE_CLASS_ID, // [BUG 2540] super.
                            oset.name);
            }
        }
        return code;
    }

    // handle each overload set
    private string generateCodeForOverloadSet(alias oset)() @property
    {
        string code = "";

        foreach (i_; CountUp!(0 + oset.contents.length)) // workaround
        {
            enum i = 0 + i_; // workaround
            code ~= generateFunction!(
                    Policy.FUNCINFO_ID!(oset.name, i), oset.name,
                    oset.contents[i]) ~ "\n";
        }
        return code;
    }

    /*
     * Returns D code which implements the function func.  This function
     * actually generates only the declarator part; the function body part is
     * generated by the functionGenerator() policy.
     */
    public string generateFunction(
            string myFuncInfo, string name, func... )() @property
    {
        import std.format : format;

        enum isCtor = (name == CONSTRUCTOR_NAME);

        string code; // the result

        auto paramsRes = generateParameters!(myFuncInfo, func)();
        code ~= paramsRes.imports;

        /*** Function Declarator ***/
        {
            alias Func = FunctionTypeOf!(func);
            alias FA = FunctionAttribute;
            enum atts     = functionAttributes!(func);
            enum realName = isCtor ? "this" : name;

            // FIXME?? Make it so that these aren't CTFE funcs any more, since
            // Format is deprecated, and format works at compile time?
            /* Made them CTFE funcs just for the sake of Format!(...) */

            // return type with optional "ref"
            static string make_returnType()
            {
                string rtype = "";

                if (!isCtor)
                {
                    if (atts & FA.ref_) rtype ~= "ref ";
                    rtype ~= myFuncInfo ~ ".RT";
                }
                return rtype;
            }
            enum returnType = make_returnType();

            // function attributes attached after declaration
            static string make_postAtts()
            {
                string poatts = "";
                if (atts & FA.pure_   ) poatts ~= " pure";
                if (atts & FA.nothrow_) poatts ~= " nothrow";
                if (atts & FA.property) poatts ~= " @property";
                if (atts & FA.safe    ) poatts ~= " @safe";
                if (atts & FA.trusted ) poatts ~= " @trusted";
                return poatts;
            }
            enum postAtts = make_postAtts();

            // function storage class
            static string make_storageClass()
            {
                string postc = "";
                if (is(Func ==    shared)) postc ~= " shared";
                if (is(Func ==     const)) postc ~= " const";
                if (is(Func ==     inout)) postc ~= " inout";
                if (is(Func == immutable)) postc ~= " immutable";
                return postc;
            }
            enum storageClass = make_storageClass();

            //
            if (__traits(isVirtualMethod, func))
                code ~= "override ";
            code ~= format("extern(%s) %s %s(%s) %s %s\n",
                    functionLinkage!(func),
                    returnType,
                    realName,
                    paramsRes.params,
                    postAtts, storageClass );
        }

        /*** Function Body ***/
        code ~= "{\n";
        {
            enum nparams = Parameters!(func).length;

            /* Declare keywords: args, self and parent. */
            string preamble;

            preamble ~= "alias args = AliasSeq!(" ~ enumerateParameters!(nparams) ~ ");\n";
            if (!isCtor)
            {
                preamble ~= "alias self = " ~ name ~ ";\n";
                if (WITH_BASE_CLASS && !__traits(isAbstractFunction, func))
                    preamble ~= "alias parent = AliasSeq!(__traits(getMember, super, \"" ~ name ~ "\"))[0];";
            }

            // Function body
            static if (WITHOUT_SYMBOL)
                enum fbody = Policy.generateFunctionBody!(name, func);
            else
                enum fbody = Policy.generateFunctionBody!(func);

            code ~= preamble;
            code ~= fbody;
        }
        code ~= "}";

        return code;
    }

    /*
     * Returns D code which declares function parameters,
     * and optionally any imports (e.g. core.vararg)
     * "ref int a0, real a1, ..."
     */
    static struct GenParams { string imports, params; }
    private GenParams generateParameters(string myFuncInfo, func...)()
    {
        alias STC = ParameterStorageClass;
        alias stcs = ParameterStorageClassTuple!(func);
        enum nparams = stcs.length;

        string imports = ""; // any imports required
        string params = ""; // parameters

        foreach (i, stc; stcs)
        {
            if (i > 0) params ~= ", ";

            // Parameter storage classes.
            if (stc & STC.scope_) params ~= "scope ";
            if (stc & STC.out_  ) params ~= "out ";
            if (stc & STC.ref_  ) params ~= "ref ";
            if (stc & STC.lazy_ ) params ~= "lazy ";

            // Take parameter type from the FuncInfo.
            params ~= format("%s.PT[%s]", myFuncInfo, i);

            // Declare a parameter variable.
            params ~= " " ~ PARAMETER_VARIABLE_ID!(i);
        }

        // Add some ellipsis part if needed.
        auto style = variadicFunctionStyle!(func);
        final switch (style)
        {
            case Variadic.no:
                break;

            case Variadic.c, Variadic.d:
                imports ~= "import core.vararg;\n";
                // (...) or (a, b, ...)
                params ~= (nparams == 0) ? "..." : ", ...";
                break;

            case Variadic.typesafe:
                params ~= " ...";
                break;
        }

        return typeof(return)(imports, params);
    }

    // Returns D code which enumerates n parameter variables using comma as the
    // separator.  "a0, a1, a2, a3"
    private string enumerateParameters(size_t n)() @property
    {
        string params = "";

        foreach (i_; CountUp!(n))
        {
            enum i = 0 + i_; // workaround
            if (i > 0) params ~= ", ";
            params ~= PARAMETER_VARIABLE_ID!(i);
        }
        return params;
    }
}


/**
Predefined how-policies for $(D AutoImplement).  These templates are also used by
$(D BlackHole) and $(D WhiteHole), respectively.
 */
template generateEmptyFunction(C, func.../+[BUG 4217]+/)
{
    static if (is(ReturnType!(func) == void))
        enum string generateEmptyFunction = q{
        };
    else static if (functionAttributes!(func) & FunctionAttribute.ref_)
        enum string generateEmptyFunction = q{
            static typeof(return) dummy;
            return dummy;
        };
    else
        enum string generateEmptyFunction = q{
            return typeof(return).init;
        };
}

/// ditto
template generateAssertTrap(C, func...)
{
    enum string generateAssertTrap =
        `throw new NotImplementedError("` ~ C.stringof ~ "."
                ~ __traits(identifier, func) ~ `");`;
}
