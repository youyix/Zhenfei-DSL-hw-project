%{
  #define YYDEBUG 1
%}

%skeleton "lalr1.cc"
%require  "3.0"
%debug 
%defines 
%define api.namespace {intergdb::lang}
%define parser_class_name {Parser}
 
%code requires{
   namespace intergdb {
     namespace lang {
        class Driver;
        class Scanner;
        
        class SchemaNode;
        class ConfNode;
        class AttrNode;

        class GraphNode;
        class EdgeNode;
        class VertexNode;

     }     
   }           
}
 
%lex-param   { Scanner  &scanner  }
%parse-param { Scanner  &scanner  }
 
%lex-param   { Driver  &driver  }
%parse-param { Driver  &driver  }
 
%code{
   #include <iostream>
   #include <cstdlib>
   #include <fstream>
   
   /* include for all driver functions */
  #include "Driver.h"
  #include "SchemaNode.h"
  #include "ConfNode.h"
  #include "AttrNode.h"
  #include "VertexNode.h"
  #include "EdgeNode.h"
  #include "GraphNode.h"

   /* this is silly, but I can't figure out a way around */
   static int yylex(intergdb::lang::Parser::semantic_type *yylval,
                    intergdb::lang::Scanner  &scanner,
                    intergdb::lang::Driver   &driver);
   
}
 
/* token types */
%union {
  std::string *sval;
  intergdb::lang::SchemaNode *schema_node;
  intergdb::lang::ConfNode *conf_node;
  intergdb::lang::AttrNode *attr_node;
  std::vector<AttrNode> *vec_attr;

  intergdb::lang::VertexNode *vertex_node;
  intergdb::lang::EdgeNode *edge_node;
  intergdb::lang::GraphNode *graph_node;
}
 

%token            END    0     "end of file" 
%token   <sval>   IDENTIFIER 
%token   <sval>   CONSTANT 
%token   <sval>   STRING_LITERAL 
%token            RIGHT_ARROW
%token   <sval>   CONSTANT_DOUBLE
%token   <sval>   CONSTANT_INT

%token            CONF
%token            NAME
%token            PATH
%token            SCHEMA
%token            VERTEX
%token            EDGE

%token            GRAPH
%token            USING

%token            LEFT_BRACE
%token            RIGHT_BRACE
%token            LEFT_PAREN
%token            RIGHT_PAREN
%token            COLON
%token            COMMA
%token            TYPE_STRING
%token            TYPE_INT64
%token            TYPE_DOUBLE

%token            SELECT
%token            EXISTS
%token            FROM
%token            ALL
%token            GT
%token            GTE
%token            LT
%token            LTE
%token            EQ
%token            NEQ

%token            AND
%token            OR
%token            NOT

%token            MINUS
%token            STAR
%token            PLUS

%type   <sval>     data_type
%type   <schema_node> vertex_schema
%type   <schema_node> edge_schema
%type   <conf_node> conf_def
%type   <vec_attr>  attribute_defs
%type   <attr_node> attribute_def
%type   <sval>      path_name
%type   <sval>      conf_name

%type   <vertex_node> vertex
%type   <edge_node>   edge
%type   <sval>        attr
%type   <sval>        constant
%type   <sval>        regular_condition
%type   <sval>        constant_num

/* destructor rule for <sval> objects */
%destructor { if ($$)  { delete ($$); ($$) = nullptr; } } <sval>
  
%%


program 
  : END 
  | query END
  | conf graph query END 
  ;

graph
  : GRAPH IDENTIFIER USING IDENTIFIER LEFT_BRACE vertex_def edge_def RIGHT_BRACE {
      GraphNode g = GraphNode(*$2, driver.variable_list_confs_.at(*$4));
      g.addVertices(driver.vertex_v);
      g.addEdges(driver.edge_v);
      driver.variable_list_graphs_.insert({*$2, g});
      driver.print(g.translate());
    }
  ;

vertex_def
  : VERTEX COLON LEFT_BRACE vertices RIGHT_BRACE {
    }
  ;

vertices
  : vertex {
      driver.vertex_v.insert(driver.vertex_v.begin(), *$1);
    }
  | vertex COMMA vertices {
      driver.vertex_v.insert(driver.vertex_v.begin(), *$1);
    }
  ;

vertex
  : CONSTANT_INT COLON attrs {
      std::vector<std::string> vv_s = driver.adnv;
      $$ = new VertexNode(*$1, vv_s);
      driver.adnv.clear();
    }
  ;

edge
  : CONSTANT_INT RIGHT_ARROW CONSTANT_INT COLON CONSTANT_DOUBLE attrs {
      std::vector<std::string> vvx_s = driver.adnv;
      $$ = new EdgeNode(*$1, *$3, *$5, vvx_s);
      driver.adnv.clear();
    }
  ;

attrs
  : attr {
      driver.adnv.insert(driver.adnv.begin(), *$1);
    }
  | attr attrs {
      driver.adnv.insert(driver.adnv.begin(), *$1);
    }
  ;

attr 
  : STRING_LITERAL {  $$ = $1; }
  | CONSTANT_INT {  $$ = $1; }
  | CONSTANT_DOUBLE {  $$ = $1; }
  ;

edge_def
  : EDGE COLON LEFT_BRACE edges RIGHT_BRACE {
      //driver.print("edge_def");
    }
  ;

edges
  : edge {
      //driver.print("edges 0");
      driver.edge_v.insert(driver.edge_v.begin(), *$1);
    }
  | edge COMMA edges {
      //driver.print("edges 1");
      driver.edge_v.insert(driver.edge_v.begin(), *$1);
    }
  ;



conf
  : CONF IDENTIFIER conf_def { 
      //std::string tmp = "Conf " + $3->toString();  
      ////driver.print(tmp);
      $3->setSelfName(*$2);
      driver.variable_list_confs_.insert( {{*$2, *$3}} );
      ////driver.print("INSERT");

      ////driver.print("**********************\n**\n**********************");
    } 
  ;

conf_def
  : LEFT_BRACE conf_name path_name schema RIGHT_BRACE {
      ConfNode *c = driver.createConf($2, $3);
      $$ = c;
    }
  ;

conf_name
  : NAME COLON STRING_LITERAL COMMA { 
      $$ = $3;
    } 
  ;

path_name 
  : PATH COLON STRING_LITERAL COMMA { 
      $$ = $3;
    } 
  ;

schema
  : SCHEMA COLON LEFT_BRACE vertex_schema edge_schema RIGHT_BRACE { 
      //driver.print("-------");
      
    }
  ;

vertex_schema
  : VERTEX COLON attribute_defs { 
      //driver.print("vertex_schema"); 
      std::vector<AttrNode> vv_s = driver.vv;
      driver.vv.clear();
      driver.v_s = driver.createSchema(&vv_s);
    }
  ;

edge_schema
  : EDGE COLON attribute_defs { 
      //driver.print("edge_schema"); 
      std::vector<AttrNode> ee_s = driver.vv;
      driver.vv.clear();
      driver.e_s = driver.createSchema(&ee_s);
    }
  ;

attribute_defs
  : attribute_def { 
      driver.vv.insert(driver.vv.begin(), *$1);
    }
  | attribute_def COMMA attribute_defs { 
      driver.vv.insert(driver.vv.begin(), *$1);
    }
  ;

attribute_def
  : LEFT_PAREN STRING_LITERAL COMMA data_type RIGHT_PAREN { 
      $$ = driver.createAttr($2, $4);
    }
  ;

data_type
  : TYPE_STRING { std::string ret = "STRING" ; $$ = &ret; }
  | TYPE_INT64 { std::string ret = "INT64" ; $$ = &ret; }
  | TYPE_DOUBLE { std::string ret = "DOUBLE" ; $$ = &ret; }
  ;
 

query
  : vertex_query {
      driver.print("Recongnized a vertex_query. ");
    //  $1->translate();
    }
  | path_query {
      driver.print("Recongnized a edge_query. ");
    //  $1->translate();
    }
  ;

vertex_query
  : SELECT VERTEX FROM IDENTIFIER LEFT_PAREN timestamps RIGHT_PAREN LEFT_BRACE bool_exprs RIGHT_BRACE {
      //driver.print("vertex_query");
    //  $$ = new VertexQuery(*$1, *$6, *$9);
    }
  ;

path_query
  : SELECT EDGE FROM IDENTIFIER LEFT_PAREN timestamps RIGHT_PAREN LEFT_BRACE search_exprs RIGHT_BRACE {
      //driver.print("path_query");
    //  $$ = new PathQuery(*$1, *$6, *$9);
    }
  ;

search_exprs 
  : search_expr {
    //  driver.r_v.insert(driver.r_v.begin(), *$1);
    }
  | search_expr COMMA search_exprs {
    //  driver.r_v.insert(driver.r_v.begin(), *$1);
    }
  ;

search_expr 
  : bool_exprs {
    //  $$ = new RegularNode("NULL", $2);
    }
  | regular_expr {
    //  $$ = $1;
    }
  ;

regular_expr
  : LEFT_BRACE bool_exprs RIGHT_BRACE regular_condition {
      //driver.print("regular_expr " + *$4);
      //$$ = new RegularNode(*$4, $2);
    }
  ;

regular_condition 
  : STAR {
      $$ = new std::string("*");
    }
  | PLUS{
      $$ = new std::string("+");
    }
  ;

bool_exprs
  : bool_expr {
      //driver.print("bool_exprs 0");
      ////$$ = new BoolNode("SINGLE", $1)
    }
  | bool_exprs AND bool_exprs {
      //driver.print("AND");
      //$$ = new BoolNode("AND", $1, $3);
    }
  | bool_exprs OR bool_exprs {
      //driver.print("OR");
      //$$ = new BoolNode("OR", $1, $3);
    }
  | NOT LEFT_PAREN bool_exprs RIGHT_PAREN {
      //driver.print("NOT");
      //$$ = new BoolNode("NOT", $1, $3);
    }
  | LEFT_PAREN bool_exprs RIGHT_PAREN {
      //driver.print("COMPOSITON");
      //$$ = $2;
    }
  ;

bool_expr 
  : comprision_pridicate {
      ////driver.print("bool_expr 0");
      //$$ = $1;
    }
  | existing_pridicate {
      ////driver.print("bool_expr 1");
      //$$ = $1;
    }
  ;

existing_pridicate
  : EXISTS LEFT_PAREN IDENTIFIER RIGHT_PAREN {
      //driver.print("EXISTS " + *$3);
      //$$ = new PredicateNode("EXISTS", *$3);
    }
  ;

comprision_pridicate
  : IDENTIFIER EQ constant {
      //driver.print(*$1 + " EQ  " + *$3);
      //$$ = new PredicateNode("EQ", *$1, *$3);
    }
  | IDENTIFIER NEQ constant {
      //driver.print(*$1 + " NEQ " +  *$3);
      //$$ = new PredicateNode("NEQ", *$1, *$3);
    }
  | IDENTIFIER GT constant {
      //driver.print(*$1 + " GT  " + *$3);
      //$$ = new PredicateNode("GT", *$1, *$3);
    }
  | IDENTIFIER GTE constant {
      //driver.print(*$1 + " GTE " +  *$3);
      //$$ = new PredicateNode("GTE", *$1, *$3);
    }
  | IDENTIFIER LT constant {
      //driver.print(*$1 + " LT  " + *$3);
      //$$ = new PredicateNode("LT", *$1, *$3);
    }
  | IDENTIFIER LTE constant {
      //driver.print(*$1 + " LTE " +  *$3);
      //$$ = new CompNode("LTE", *$1, *$3);
    }
  ;

constant
  : CONSTANT_INT { $$ = $1;}
  | CONSTANT_DOUBLE { $$ = $1;}
  | STRING_LITERAL { $$ = $1;}
  | CONSTANT { $$ = $1;}
  ;

constant_num
  : CONSTANT_DOUBLE { $$ = $1;}
  | CONSTANT_INT { $$ = $1;}
  ;

timestamps
  : ALL {
      //driver.print("TS: all");
      //new std::pair<std::string, std::string>("*", "*");
    }
  | constant_num MINUS constant_num {
      //driver.print("TS: " + *$1 + " - " + *$3);
      //new std::pair<std::string, std::string>(*$1, *$3);
    }
  ;

%%
 
 
void 
intergdb::lang::Parser::error( const std::string &err_message )
{
   std::cerr << "Error!: " << err_message << "\n"; 
}
 
 
/* include for access to scanner.yylex */
#include "Scanner.h"
static int 
yylex( intergdb::lang::Parser::semantic_type *yylval,
       intergdb::lang::Scanner  &scanner,
       intergdb::lang::Driver   &driver )
{
   return( scanner.yylex(yylval) );
}

