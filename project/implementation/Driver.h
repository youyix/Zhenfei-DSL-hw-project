#ifndef __DRIVER_H__
#define __DRIVER_H__ 1

#include <string>
#include "Scanner.h"
#include "Parser.tab.hh"

#include "SchemaNode.h"
#include "ConfNode.h"
#include "AttrNode.h"

#include "GraphNode.h"
#include "VertexNode.h"
#include "EdgeNode.h"

#include <unordered_map>

namespace intergdb {
    namespace lang {
   
        class SchemaNode;
        class ConfNode;
        class AttrNode;

        class VertexNode;
        class GraphNode;
        class EdgeNode;

        class Driver {    
        public:
        Driver() : 
            parser( nullptr ),
            scanner( nullptr ){};
            virtual ~Driver();

            void parse( const char *filename );  
            void printVertex();
            void printEdge();

            void print(std::string sth);
            void printCreateGraph(std::string *graph_name);

            SchemaNode* createSchema(std::vector<AttrNode> *v);
            ConfNode* createConf(std::string *name, std::string *type);
            AttrNode* createAttr(std::string *name, std::string *type);

            // VertexNode* createVertex(std::string id, std::vector<std::string> *attrs);
            // EdgeNode* createEdge(std::string u, std::string v, 
            //     std::string ts, std::vector<std::string> *attrs);

            // GraphNode* createGraph(ConfNode &conf);

            void test();
            // ugly temp variable
            SchemaNode *v_s;
            SchemaNode *e_s;

            std::vector<AttrNode> vv;

            std::unordered_map<std::string, ConfNode&> variable_list_confs_;
            std::unordered_map<std::string, GraphNode&> variable_list_graphs_;     

            //
            std::vector<std::string> adnv;  
            std::vector<VertexNode> vertex_v;  
            std::vector<EdgeNode> edge_v;  

            // std::vector<RegularNode> r_v;    

        private:
            Parser *parser;
            Scanner *scanner;


            
        };

    } /* end namespace lang */
} /* end namespace intergdb */

#endif /* END __DRIVER_H__ */
