class CheckSemant() extends CoolTreeVisitor()
{
  var native_sym : Symbol = symbol("native");
  var null_sym : Symbol = symbol("Null");
  var nothing_sym : Symbol = symbol("Nothing");

  override def visit_Program(n:Program) : Any = {
    check_non_null(n,"any_class",n.get_any_class());
    check_non_null(n,"int_class",n.get_int_class());
    check_non_null(n,"unit_class",n.get_unit_class());
    check_non_null(n,"boolean_class",n.get_boolean_class());
    check_non_null(n,"string_class",n.get_string_class())
  };

  override def visit_class_decl(the_node:Cclass_decl,name:Symbol,parent:Symbol,features:Features,filename:Symbol) : Any = {
    if (parent == native_sym) null
    else check_non_null(the_node,"superclass",the_node.get_superclass());
    super.visit_class_decl(the_node,name,parent,features,filename)
  };

  override def visit_method(the_node:Cmethod,overridep:Boolean,name:Symbol,formals:Formals,return_type:Symbol,expr:Expression) : Any = {
    check_non_null(the_node,"owner",the_node.get_owner());
    if (return_type == null_sym) null
    else if (return_type == nothing_sym) null
    else check_non_null(the_node,"(method)feature_of_class",
			the_node.get_feature_of_class());
    super.visit_method(the_node,overridep,name,formals,return_type,expr)
  };

  override def visit_attr(the_node:Cattr,name:Symbol,of_type:Symbol) : Any = {
    check_non_null(the_node,"owner",the_node.get_owner());
    if (of_type == null_sym) null
    else if (of_type == nothing_sym) null
    else check_non_null(the_node,"(attr)feature_of_class",
			the_node.get_feature_of_class());
    super.visit_attr(the_node,name,of_type)
  };

  override def visit_formal(the_node:Cformal,name:Symbol,of_type:Symbol) : Any = {
    if (of_type == null_sym) null
    else if (of_type == nothing_sym) null
    else check_non_null(the_node,"formal_of_class",
			the_node.get_formal_of_class());    
    super.visit_formal(the_node,name,of_type)
  };

  override def visit_branch(the_node:Cbranch,name:Symbol,local_type:Symbol,expr:Expression) : Any = {
    check_non_null(the_node,"case_of_type",the_node.get_case_of_type());
    if (local_type == null_sym) null
    else check_non_null(the_node,"case_of_class",
			the_node.get_case_of_class());    
    super.visit_branch(the_node,name,local_type,expr)
  };

  override def visit_Expression(the_node:Expression) : Any = {
    check_non_null(the_node,"of_type",the_node.get_of_type());
    super.visit_Expression(the_node)
  };

  override def visit_let(the_node:Clet,identifier:Symbol,local_type:Symbol,init:Expression,body:Expression) : Any = {
    if (local_type == null_sym) null
    else if (local_type == nothing_sym) null
    else check_non_null(the_node,"of_class",the_node.get_of_class());
    super.visit_let(the_node,identifier,local_type,init,body)
  };

  override def visit_alloc(the_node:Calloc,type_name:Symbol) : Any = {
    check_non_null(the_node,"of_class",the_node.get_of_class());
    super.visit_alloc(the_node,type_name)
  };

  override def visit_assign(the_node:Cassign,name:Symbol,expr:Expression) : Any = {
    check_non_null(the_node,"binding",the_node.get_binding());
    super.visit_assign(the_node,name,expr)
  };

  override def visit_variable(the_node:Cvariable,name:Symbol) : Any = {
    check_non_null(the_node,"binding",the_node.get_binding());
    super.visit_variable(the_node,name)
  };

  override def visit_static_dispatch(the_node:Cstatic_dispatch,expr:Expression,type_name:Symbol,name:Symbol,actuals:Expressions) : Any = {
    check_non_null(the_node,"mbinding",the_node.get_mbinding());
    super.visit_static_dispatch(the_node,expr,type_name,name,actuals)
  };

  override def visit_dispatch(the_node:Cdispatch,expr:Expression,name:Symbol,actuals:Expressions) : Any = {
    check_non_null(the_node,"mbinding",the_node.get_mbinding());
    super.visit_dispatch(the_node,expr,name,actuals)
  };

  def check_non_null(n : CoolNode, aname : String, binding : Any) : Any =
    if (is_null(binding))
      out("Attribute ").out_any(n).out(".").out(aname).out(" undefined.\n")
    else null;
}