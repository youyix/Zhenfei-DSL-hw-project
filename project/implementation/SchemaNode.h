#ifndef __SCHEMANODE_H__
#define __SCHEMANODE_H__ 1

#include <string>
#include <vector>
#include <iostream> 
#include <sstream> 
#include <unordered_map>    
// #include <boost/variant.hpp>

// #include <intergdb/lang/AttrNode.h>
// 
#include "AttrNode.h"


namespace intergdb { namespace lang
{
    class AttrNode;
    class SchemaNode {
    public:
        friend class AttrNode;

        // enum DataType { INT64, DOUBLE, STRING };

        // SchemaNode(){};
        SchemaNode(std::vector<AttrNode> &v);
        void addAttr(AttrNode &attr);   

        std::string toString();

        std::string translate();
    private:
        std::vector<AttrNode> attributes_;            /* list of name/type pairs */
        // std::unordered_map<std::string, int> nameToIndex_; /* map from name to index */
    };

    inline std::ostream& operator<<(std::ostream &os, SchemaNode & node)
    {      
       return os << node.toString();
    }

} } /* namespace */


#endif