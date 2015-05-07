#ifndef __CONFNODE_H__
#define __CONFNODE_H__ 1

#include <string>
#include <vector>
#include <iostream> 
#include <sstream>     

// #include <intergdb/lang/SchemaNode.h>
#include "SchemaNode.h"
// #include "AttrNode.h"

namespace intergdb { namespace lang
{

    class ConfNode {
    public:
        // ConfNode(){};
        ConfNode(std::string name, std::string path, 
            SchemaNode &v_schema, SchemaNode &e_schema)
            : 
            name_(name), 
            db_path_(path), 
            vertex_schema_(v_schema),
            edge_schema_(e_schema)
            {};

        std::string translate();

        void setSelfName(std::string self_name) {
            s_name_ = self_name;
        };

        std::string getSelfName() {
            return s_name_;
        };


        std::string toString() {
            std::string k = "Conf " + name_ + " " + db_path_ + "\n";
            std::stringstream ss;
            ss << "vertex_schema" << std::endl << vertex_schema_ << std::endl;
            ss << "edge_schema" << std::endl << edge_schema_ << std::endl;
            return k + ss.str();
        }
    private:
        std::string s_name_;
        std::string name_;
        std::string db_path_;
        SchemaNode & vertex_schema_;
        SchemaNode & edge_schema_;
    };

    inline std::ostream& operator<<(std::ostream &os, ConfNode & node)
    {      
       return os << node.toString();
    }
} } /* namespace */


#endif