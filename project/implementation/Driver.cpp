#include <cctype>
#include <fstream>
#include <cassert>
#include <iostream>

#include "driver.h"
#include "SchemaNode.h"
#include "ConfNode.h"
#include "AttrNode.h"

using namespace intergdb;
using namespace intergdb::lang;

Driver::~Driver(){ 
   delete(scanner);
   scanner = nullptr;
   delete(parser);
   parser = nullptr;
}

void 
Driver::parse( const char *filename )
{
   assert( filename != nullptr );
   std::ifstream in_file( filename );
   if( ! in_file.good() ) exit( EXIT_FAILURE );
   
   delete(scanner);
   try
   {
      scanner = new Scanner( &in_file );
   }
   catch( std::bad_alloc &ba )
   {
      std::cerr << "Failed to allocate scanner: (" <<
         ba.what() << "), exiting!!\n";
      exit( EXIT_FAILURE );
   }
   
   delete(parser); 
   try
   {
      parser = new Parser( (*scanner) /* scanner */, 
                                  (*this) /* driver */ );
   }
   catch( std::bad_alloc &ba )
   {
      std::cerr << "Failed to allocate parser: (" << 
         ba.what() << "), exiting!!\n";
      exit( EXIT_FAILURE );
   }
   const int accept( 0 );
   if( parser->parse() != accept )
   {
      std::cerr << "Parse failed!!\n";
   }
}

void 
Driver::printVertex()
{ 
    std::cout << "vertex" << std::endl;
}

void 
Driver::printEdge()
{ 
    std::cout << "edge" << std::endl;
}

void 
Driver::printCreateGraph(std::string *graph_name)
{
    std::cout << "graph " << *graph_name << std::endl;
}

void 
Driver::print(std::string sth)
{
    std::cout << "//" << sth << std::endl;
}

SchemaNode* 
Driver::createSchema(std::vector<AttrNode> *v)
{
    // std::cout << "A SchemaNode created." << std::endl;
    return new SchemaNode(*v);
}

ConfNode* 
Driver::createConf(std::string *name, std::string *type)
{
    // std::cout << "A ConfNode created." << std::endl;
    return new ConfNode(*name, *type, *v_s, *e_s);
}


AttrNode* 
Driver::createAttr(std::string *name, std::string *type)
{   
    // std::cout << "An AttrNode created.(" << *name << ", " << *type << ")\n";
    return new AttrNode(*name, *type);
}

void
Driver::test()
{
    // for (auto& x: variable_list_confs_) {
    //     std::cout << x.first << "::: \n" << x.second << std::endl;
    // }

    ///////
    // std::cout << "\n\n******\n\n" << std::endl;
}


// VertexNode* 
// createVertex(std::string id, std::vector<std::string> *attrs)
// {
//     std::cout << " vvv " << std::endl;

//     // return new VertexNode(0, *attrs);
//     return nullptr;
// }

// EdgeNode* 
// createEdge(std::string u, std::string v, 
//     std::string ts, std::vector<std::string> *attrs)
// {
//     // std::cout << " eee " << std::endl;

//     // return new EdgeNode(0, 1, 3.3, *attrs);
//     return nullptr;
// }

// GraphNode* 
// createGraph(ConfNode &conf)
// {
//     std::cout << " ggg " << std::endl;

//     return new GraphNode(conf);
// }








































