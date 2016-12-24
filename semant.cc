

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"


extern int semant_debug;
extern char *curr_filename;
const std::string desperateErrMsg("This is embarassing. Something went wrong and I don't know what it is. =/");

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

ClassTable& getClassTable() {
    static ClassTable gClassTable;

    return gClassTable;

}

ClassTable::ClassTable() : semant_errors(0), error_stream(cerr), _classes(NULL), _curEnvironment(NULL){}

void ClassTable::install_classes(Classes classes_) {
    _classes = classes_;
    install_basic_classes();
    install_tree_classes(classes_);

    if (_classMap.find(Main) == _classMap.end() ) {
        semant_error();
        throw SemantException("Main Class was not defined");
    }
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    this->insert_class(Object, Object_class);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    this->insert_class(IO, IO_class);

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    this->insert_class(Int, Int_class);

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    this->insert_class(Bool, Bool_class);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

    this->insert_class(Str, Str_class);
}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 




void ClassTable::verify_heritage() {
    SymbolSet stack; 
    
    for (ClassMap::iterator it = _classMap.begin(); it != _classMap.end(); ++it) {
        if (is_cycle(it->second, stack)) {
            this->semant_error(it->second);
            throw SemantException("Cycle in inheritance graph");
        }
    }
}

void ClassTable::set_environments() {

    bool foundMainMeth = false;

    for (ClassMap::iterator it = _classMap.begin(); it != _classMap.end(); ++it) {
        Class_ c = it->second;
        c->setEnvironment();
    }

    //Now looks for main method
    for (ClassMap::iterator it = _classMap.begin(); it != _classMap.end(); ++it) {
        Class_ c = it->second;
        if (c->getName() != Main) continue;
        Environment& e = c->getEnvironment();
        MethodMap::iterator jt = e.methodTbl.find(main_meth);
        if (jt != e.methodTbl.end()) {
            if (jt->second->getFormals()->len() == 0) {
                foundMainMeth = true;
            }
        }
    }

    if (!foundMainMeth) {
        semant_error();
        throw SemantException("Main class doesn't have properly defined main method");
    }
}

void class__class::setEnvironment() {

    for(int ii = features->first(); features->more(ii); ii = features->next(ii)) {
        features->nth(ii)->setEnvironment(_env);
    }
}

void method_class::setEnvironment(Environment& env_) {

    if (name == self) {
        getClassTable().semant_error(name,this);
        throw SemantException("Invalid use of self");
    }


    if (env_.methodTbl.find(name) != env_.methodTbl.end()) {
        getClassTable().semant_error(name, this);
        throw SemantException("Multiple definition of method: " + std::string(name->get_string()));
    }

    env_.methodTbl[name] = this;
}

void attr_class::setEnvironment(Environment& env_) {
    if (name == self) {
        getClassTable().semant_error(name,this);
        throw SemantException("Invalid use of self");
    }

    if (env_.objectTbl.probe(name)) {
        getClassTable().semant_error(name, this);
        throw SemantException("Multiple declaration of symbol: " + std::string(name->get_string()));
    }

    env_.objectTbl.addid(name, new Symbol(type_decl));

}

bool ClassTable::is_cycle(Class_ c, SymbolSet& stack_) {
    Symbol s = c->getName(), ps = c->getParent();
    Class_ pc = NULL;
    //cout << s << " " << ps << endl;


    if (ps == Int || ps == Str || ps == Bool) {
        this->semant_error(c);
        throw SemantException("It is an error to inherit from Int, Str or Bool classes.");
    }
    
    ClassMap::iterator it = _classMap.find(ps);
    if (ps != No_class) {
        if (it == _classMap.end()) {
            this->semant_error(c);
            throw SemantException("Undefined parent class");
        }
        pc = it->second;
    }

    const bool visited = _visited.count(s) > 0;
    const bool visited_parent = _visited.count(ps) > 0;

    if (!visited) {
        _visited.insert(s);
        stack_.insert(s);
        if (pc != NULL) {
            if (!visited_parent && is_cycle(pc, stack_)) {
                return true;
            } else if (stack_.count(ps) > 0) {
                return true;
            }
        }
        
    }
    stack_.clear();
    return false;
}

void ClassTable::install_tree_classes(Classes classes_) {

    //install the other classes from AST
    for(int ii = classes_->first(); classes_->more(ii); ii = classes_->next(ii)) {
        //cout << "Installed class " << classes_->nth(ii)->getName() << endl;
        this->insert_class(classes_->nth(ii)->getName(), classes_->nth(ii));
    }

}

void ClassTable::insert_class(Symbol sym_, Class_ class_) {

    if (_classMap.find(sym_) != _classMap.end()) {
        this->semant_error(class_);
        throw SemantException("multiple definition of " + std::string(sym_->get_string()) + "class");
    }

    //TODO - If class name is SELF_TYPE, throw exception too

    //Else insert element in map =)

    _classMap[sym_] = class_;
    
}

Class_ ClassTable::getClass(Symbol s) {
    Symbol actualSym = s;
    if (s == SELF_TYPE ) {
        actualSym = _curEnvironment->_class->getName();
    }
    //assert(actualSym);

    ClassMap::iterator it = _classMap.find(actualSym);

    if (it == _classMap.end()) {
        /*if (actualSym) { // corner case when a NULL symbom (no_type) gets passed
            semant_error(_curEnvironment->_class);
            throw SemantException("Reference to a non-existent type: " + std::string(s->get_string()));
        } else {
            //TODO - read commented line below - this will cause problems with err reporting
            ///std::cout << "Something interesting is going on in class : " << _curEnvironment->_class->getName() << std::endl;
            semant_error(_curEnvironment->_class);
            throw SemantException("Compiler internal error");
        }*/
        return NULL;
    }

    return it->second;

}

Symbol ClassTable::getAttrType(Symbol class_, Symbol attr) {
    Class_ c = this->getClass(class_);
    if (c == NULL) {
        semant_error(_curEnvironment->_class);
        throw SemantException(desperateErrMsg);
    }
    SymbolTbl& env = c->getEnvironment().objectTbl;
    Symbol* it = env.lookup(attr);

    if (it == NULL) { /* looks up in parent classes */
        if (c->getParent() == No_class ) {
            return NULL;
        } else {
            return this->getAttrType(c->getParent(), attr);
        }
    }

    return *it;
    
}

Feature ClassTable::getMethod(Symbol class_, Symbol method ) {
    Class_ c = this->getClass(class_);
    if (c == NULL) {
        semant_error(_curEnvironment->_class);
        throw SemantException(desperateErrMsg);
    }
    MethodMap& env = c->getEnvironment().methodTbl;

    MethodMap::iterator it = env.find(method);

    if (it == env.end()) { /* looks up in parent classes */
        if (c->getParent() == No_class ) {
            return NULL;
        } else {
            return this->getMethod(c->getParent(), method);
        }

    }
    return it->second;
}

bool ClassTable::is_descendant(Symbol s1, Symbol s2) {
    Class_ c1 = this->getClass(s1);
    Class_ c2 = this->getClass(s2);
    if (c1 == NULL || c2 == NULL) {
        semant_error(_curEnvironment->_class);
        throw SemantException(desperateErrMsg);
    }
    if ((c1 == c2)) { 
        return true;
    } else if (c1->getParent() == No_class) {
        return false;
    } else {
        return is_descendant(c1->getParent(), s2);
    }

}

Symbol ClassTable::least_ub(Symbol s1, Symbol s2) {
    Class_ c1 = this->getClass(s1);
    Class_ c2 = this->getClass(s2);
    if (c1 == NULL || c2 == NULL) {
        semant_error(_curEnvironment->_class);
        throw SemantException(desperateErrMsg);
    }
    s1 = c1->getName();
    s2 = c2->getName();

    if (is_descendant(s1,s2)) { 
        return s2; 
    } else if (is_descendant(s2,s1)) {
        return s1;
    } else {
        return least_ub(c1->getParent(), s2);
    }

}

bool ClassTable::comp_signature(Formals formals_, Expressions expressions_) {
    if (formals_->len() != expressions_->len()) return false;

    for (int ii = formals_->first(), jj = expressions_->first(); 
            formals_->more(ii),expressions_->more(jj); 
            ii = formals_->next(ii), jj = expressions_->next(jj) ) {
       
       Symbol actualType = (expressions_->nth(jj)->get_type() == SELF_TYPE) ? 
                            _curEnvironment->_class->getName() : 
                            expressions_->nth(jj)->get_type(); 
                           
       if (formals_->nth(ii)->getType() != actualType) {
            return false;
       }
    }

    return true;
}

void Environment::addEntry(Symbol s1, Symbol s2) {
    SymbolTbl& env = objectTbl;
    ClassMap& classMap = getClassTable().getClassMap();

    //TODO - reject SELF_TYPE

    if (classMap.find(s2) == classMap.end() ) {
        getClassTable().semant_error(_class);
        throw SemantException("Formal type" + std::string(s2->get_string()) +
                "doesn't exist" );
    }

    if (env.probe(s1)) {
        getClassTable().semant_error(_class);
        throw SemantException("Multiple declaration of symbol: " + std::string(s2->get_string()));
    }

    env.addid(s1, new Symbol(s2));

}

void ClassTable::analyze() {
        for(int ii = _classes->first(); _classes->more(ii); ii = _classes->next(ii)) {
            //cout << "Installed class " << classes_->nth(ii)->getName() << endl;
            _curEnvironment = &_classes->nth(ii)->getEnvironment();
            _curEnvironment->_class = _classes->nth(ii);
            if (_curEnvironment == NULL) {
                semant_error(_curEnvironment->_class);
                throw SemantException("Compiler internal error");
            }
            _classes->nth(ii)->analyze();
        }

}


//class name inherits parent { ...features...};
//class C inherits D { ...};
void class__class::analyze() {

    for (int ii = features->first(); features->more(ii); ii = features->next(ii)) {
        try {
            features->nth(ii)->analyze();
        } catch(SemantException& se) {
            std::cerr << se.what() << std::endl;
        }
    }

}

// name (formals) : return_type {expr}
// foo(x : Int, y: Bool) : C {...}
void method_class::analyze() {
    SymbolTbl& env = getClassTable().getEnvironment()->objectTbl;
    env.enterscope();

    for (int ii = formals->first(); formals->more(ii); ii = formals->next(ii)) {
        formals->nth(ii)->analyze();
    }

    try {
        expr->analyze();
    } catch(SemantException& se) {
        std::cerr << se.what() << std::endl;
    }

    env.exitscope();

    //cout << expr->get_type() << " " << return_type << endl;

    if (!getClassTable().is_descendant(expr->get_type(), return_type)) {
        getClassTable().semant_error(return_type, this);
        throw SemantException("Incompatible return type " + std::string(return_type->get_string()));
    }
}

//name : type_decl <- init;
//a : Int <- 0;
void attr_class::analyze() {

    init->analyze();

    //cout << init->get_type() << " " << type_decl << endl;
    if (init->get_type() == No_type) return; //No init expressions

    if (!getClassTable().is_descendant(init->get_type(), type_decl)) {
        getClassTable().semant_error(type_decl, this);
        throw SemantException("Incompatible initialization type " + 
                std::string(type_decl->get_string()));
    }

}

//(name : type_decl)
//(A : Int)
void formal_class::analyze() {
    SymbolTbl& env = getClassTable().getEnvironment()->objectTbl;
    ClassMap& classMap = getClassTable().getClassMap();

    //TODO - reject SELF_TYPE

    if (classMap.find(type_decl) == classMap.end() ) {
        getClassTable().semant_error(type_decl, this);
        throw SemantException("Formal type" + std::string(type_decl->get_string()) +
                "doesn't exist" );
    }

    if (name == self) {
        getClassTable().semant_error(name,this);
        throw SemantException("Invalid use of self");
    }

    if (env.probe(name)) {
        getClassTable().semant_error(name, this);
        throw SemantException("Multiple declaration of symbol: " + std::string(name->get_string()));
    }

    env.addid(name, new Symbol(type_decl));
}

//<name> : <type_decl> => <expr>
void branch_class::analyze() {
    SymbolTbl& env = getClassTable().getEnvironment()->objectTbl;
    ClassMap& classMap = getClassTable().getClassMap();

    env.enterscope();

    //TODO - reject SELF_TYPE

    if (classMap.find(type_decl) == classMap.end() ) {
        getClassTable().semant_error(type_decl, this);
        throw SemantException("Formal type" + std::string(type_decl->get_string()) +
                "doesn't exist" );
    }

    if (name == self) {
        getClassTable().semant_error(name,this);
        throw SemantException("Invalid use of self");
    }

    if (env.probe(name)) {
        getClassTable().semant_error(name, this);
        throw SemantException("Multiple declaration of symbol: " + std::string(name->get_string()));
    }

    env.addid(name, new Symbol(type_decl));

    expr->analyze();

    env.exitscope();
}

//name <- expr;
void assign_class::analyze() {
    Class_ class_ = getClassTable().getEnvironment()->_class;

    expr->analyze();
    
    if (expr->get_type() != getClassTable().getAttrType(class_->getName(),name) ) {
        std::stringstream ss("");
        ss << "Expression being assigned (" << expr->get_type() << ") doesn't match " <<
            "attribute type (" << getClassTable().getAttrType(class_->getName(),name) << ")";
        getClassTable().semant_error(class_->get_filename(),this);
        throw SemantException(ss.str());
    }
    
    type = expr->get_type();
}

//<expr>@<type_name>.name(actual)
//c@Foo.bar(a,b);
void static_dispatch_class::analyze() {
    Class_ class_ = getClassTable().getEnvironment()->_class;

    expr->analyze();

    for (int ii = actual->first(); actual->more(ii); ii = actual->next(ii)) {
        actual->nth(ii)->analyze();
    }
    Feature method = getClassTable().getMethod(type_name, name);
    if (method == NULL) {
        std::stringstream ss;
        ss << "Method " << name << " doesn't exist";
        getClassTable().semant_error(class_->get_filename(), this);
        throw SemantException(ss.str()); 
    }
    
    if (!getClassTable().comp_signature(method->getFormals(), actual)) {
        std::stringstream ss;
        ss << "Method " << name << " is being called with wrong arguments";
        getClassTable().semant_error(class_->get_filename(), this);
        throw SemantException(ss.str()); 
    }
    type = method->getReturnType();
}

//<expr>.name(actual)
//c.bar(a,b);
void dispatch_class::analyze() {
    Class_ class_ = getClassTable().getEnvironment()->_class;

    expr->analyze();

    for (int ii = actual->first(); actual->more(ii); ii = actual->next(ii)) {
        actual->nth(ii)->analyze();
    }

    Feature method = getClassTable().getMethod(expr->get_type(), name);
    if (method == NULL) {
        std::stringstream ss;
        ss << "Method " << name << " doesn't exist";
        getClassTable().semant_error(class_->get_filename(), this);
        throw SemantException(ss.str()); 
    }

    if (!getClassTable().comp_signature(method->getFormals(), actual)) {
        std::stringstream ss;
        ss << "Method " << name << " is being called with wrong arguments";
        getClassTable().semant_error(class_->get_filename(), this);
        throw SemantException(ss.str()); 
    }

    type =  method->getReturnType();

}

//if <pred> then <then_exp> else <else_exp>
void cond_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    pred->analyze();

    then_exp->analyze();

    else_exp->analyze();

    if (pred->get_type() != Bool) {
        getClassTable().semant_error(filename, this);
        throw SemantException("Predicate is not of Bool type.");
    }

    type = getClassTable().least_ub(then_exp->get_type(),else_exp->get_type());
}

//while <pred> loop <body> pool
void loop_class::analyze() {
    pred->analyze();

    body->analyze();

    if (pred->get_type() != Bool) {
        getClassTable().semant_error();
        throw SemantException("Predicate is not of Bool type.");
    }

    type = Object;
}

//case <expr> of <cases> esac
//case e of a : Foo => a.bar(); b : Bar => b.foo();
void typcase_class::analyze() {
    type = Object;
    expr->analyze();
    int ii = 0;

    //Type of a case expression is the "avg" of the types presented in the branches,
    //so let's do that with least upper bound

    if (cases->len() > ii) {
        cases->nth(ii)->analyze();
        type = cases->nth(ii)->get_type();
        ii = cases->next(ii);
    }
    
    for (; cases->more(ii); ii = cases->next(ii)) {
        cases->nth(ii)->analyze();
        type = getClassTable().least_ub(type,cases->nth(ii)->get_type());
    }

    //TODO - check for duplicates now - as there can't be any

}

//{body}
//{ expr1; expr2; exprn;}
void block_class::analyze() {

    for (int ii = body->first(); body->more(ii); ii = body->next(ii)) {
        try {
            body->nth(ii)->analyze();
            type = body->nth(ii)->get_type();
        } catch(SemantException& se) {
            std::cerr << se.what() << std::endl;
            type = Object;
        }
    }

}

//let <identifier> : <type_decl> [<- init] in <body>
void let_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    Environment* env = getClassTable().getEnvironment();

    env->objectTbl.enterscope();
    init->analyze();
    env->addEntry(identifier,type_decl);

    body->analyze();

    if ((init->get_type() == No_type) ||
        (getClassTable().is_descendant(init->get_type(), type_decl))) {    
        type = body->get_type();
    } else {
        std::stringstream ss;
        ss << init->get_type() << " type cannot be assigned to static type "  <<
            type_decl;
        getClassTable().semant_error(filename,this);
        throw SemantException(ss.str());
    }

    env->objectTbl.exitscope();

}

// e1 + e2
void plus_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    e1->analyze();
    e2->analyze();

    if (e1->get_type() != Int || e2->get_type() != Int) {
        getClassTable().semant_error(filename, this);
        throw SemantException("Expression isn't integer");
    }

    type = Int;
}

// e1 - e2
void sub_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    
    e1->analyze();
    e2->analyze();

    if (e1->get_type() != Int || e2->get_type() != Int) {
        getClassTable().semant_error(filename, this);
        throw SemantException("Expression isn't integer");
    }

    type = Int;
}

// e1 / e2
void divide_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    
    e1->analyze();
    e2->analyze();

    if (e1->get_type() != Int || e2->get_type() != Int) {
        getClassTable().semant_error(filename, this);
        throw SemantException("Expression isn't integer");
    }
    type = Int;
}

// e1 * e2
void mul_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    
    e1->analyze();
    e2->analyze();
    if (e1->get_type() != Int || e2->get_type() != Int) {
        getClassTable().semant_error(filename, this);
        throw SemantException("Expression isn't integer");
    }
    type = Int;
}

//~e1
void neg_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    
    e1->analyze();

    if (e1->get_type() != Int) {
        getClassTable().semant_error(filename, this);
        throw SemantException("Expression isn't integer");
    }
    type = Int;
}

//e1 < e2
void lt_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    
    e1->analyze();
    e2->analyze();
    if (e1->get_type() != Int) {
        getClassTable().semant_error(filename,this);
        throw SemantException("Expression isn't integer");
    }
    type = Bool;
}

//e1 = e2
void eq_class::analyze() {
    
    e1->analyze();
    e2->analyze();

    //TODO - better type check above expressions first!
    //..
    //..

    type = Bool;
}

//e1 <= e2
void leq_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    
    e1->analyze();
    e2->analyze();
    if (e1->get_type() != Int || e2->get_type() != Int) {
        getClassTable().semant_error(filename,this);
        throw SemantException("Expression isn't integer");
    }
    type = Bool;
}

//NOT e1
void comp_class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();
    
    e1->analyze();
    if (e1->get_type() != Bool) {
        getClassTable().semant_error(filename,this);
        throw SemantException("Expression isn't Bool");
    }
    type = Bool;
}

//666
void int_const_class::analyze() {
    type = Int;
}

//true, false
void bool_const_class::analyze() {
    type = Bool;
}

//"Milton"
void string_const_class::analyze() {
    type = Str;
}

//new type_name
void new__class::analyze() {
    Symbol filename = getClassTable().getEnvironment()->_class->get_filename();

    if (getClassTable().getClass(type_name) == NULL) {
       std::stringstream ss;
       ss << "Dynamic type " << type_name << " was not defined";
       getClassTable().semant_error(filename,this); 
       throw SemantException(ss.str());
    }

    type = type_name;
}

//isvoid e1
void isvoid_class::analyze() {
    e1->analyze();
    type = Bool;
}

void no_expr_class::analyze() {
    type = No_type;
}

void object_class::analyze() {
    if (name == self) {
        type = SELF_TYPE;
    } else {
        type = getClassTable().getAttrType(getClassTable().getEnvironment()->_class->getName(),name);
    }
}

/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{

    /* ClassTable constructor may do some semantic analysis */
    ClassTable& classtable = getClassTable();

    try {
        initialize_constants();
        classtable.install_classes(classes);

        //First, verify if inheritance graph is valid...
        classtable.verify_heritage();

        //Then, set environment for all the classes 
        classtable.set_environments();
        
        classtable.analyze();


        //Then, do the rest...


        /* some semantic analysis code may go here */ 
        //...
        //...
    } catch (SemantException& se) {
        cerr << se.what() << std::endl;
    } catch (std::exception& e) {
        cerr << "Standard Exception thrown: " << e.what() << endl;
    }

    if (classtable.errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }
}
