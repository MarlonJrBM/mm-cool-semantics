#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include "cool-tree.h"
#include "stringtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;


// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  ostream& error_stream;
  Classes _classes;

  ClassMap _classMap;
  SymbolSet _visited;
  void install_basic_classes();
  void install_tree_classes(Classes classes_);

  Environment* _curEnvironment;

  /**
   * Inserts class in class map, doing some basic checking.
   * Will set errors accordingly and throw SemantException if detects any malformation.
   */
  void insert_class(Symbol, Class_);

  /**
   * Returns true if a cycle is found in inheritance graph.
   * Helper function to verify if there is a cycle in inheritance graph
   *
   */
  bool is_cycle(Class_, SymbolSet&);




public:
  ClassTable(Classes);
  ClassTable();
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);

  /*
   * Performs recursive analysis on AST elements
   */
  void analyze();

  /**
   * Installs classes in current ClassTable instance.
   * Throws exception if it finds any malformation
   */
  void install_classes(Classes classes_);

  /**
   * Verifies if there are no cycles in inheritance graph of the AST.
   * If there is at least one, will set errors accordingly and throw SemantException.
   */
  void verify_heritage(); 

  /**
   * Set environment of each class.
   * Throws SemantException error on any formation.
   *
   */
  void set_environments();

  /**
   * Given a symbol returns associated class.
   * If symbol refers to a class which doesn't exist, returns NULL
   */
  Class_ getClass(Symbol);

  /**
   * Given two symbols (first referring to class, and second to attribute), returns the 
   * attributte's type.
   * If symbol refers to a class or attr which doesn't exist, throws SemantException
   */
  Symbol getAttrType(Symbol,Symbol);

  /**
   * Given two symbols (first referring to class, and second to method), returns the 
   * method.
   * If symbol refers to a class or method which doesn't exist, throws SemantException
   */
  Feature getMethod(Symbol,Symbol);

  /**
   * Returns true if first class is a descendant of second, false otherswise.
   *
   */
  bool is_descendant(Symbol, Symbol);

  /*
   * Returns least upper bound for two given classes.
   * In worst case, will return Object symbol
   */
  Symbol least_ub(Symbol, Symbol);

  /**
   * Returns true if signature required by Formals is the the same provided by
   * Expresions.
   */
  bool comp_signature(Formals, Expressions);

  Environment* getEnvironment() { return _curEnvironment; }
  ClassMap& getClassMap() { return _classMap; }

};

/**
* Retrieves the one and only ClassTable Instance
*/

ClassTable& getClassTable();

#endif

