#ifndef __ATTRNODE_H__
#define __ATTRNODE_H__ 1

#include <string>
#include <vector>
#include <iostream> 
#include <sstream>     
// #include <boost/variant.hpp>
// #include <intergdb/lang/SchemaNode.h>
#include "SchemaNode.h"

// using namespace 



namespace intergdb { namespace lang
{
    enum DataType { INT64, DOUBLE, STRING };

    static char const * dataTypesStrings[] = { "INT64", "DOUBLE", "STRING" };        
    // static char const * dataTypesStrings[] = { "INT64", "DOUBLE", "STRING" };        
    // static char const * typeToString(int enumVal)
    // {
    //     return dataTypesStrings[enumVal];
    // }

    class SchemaNode;
    class AttrNode {
    public:

        AttrNode(std::string name, DataType type);
        AttrNode(std::string name, std::string type);

        std::string translate();

        std::string toString();
    private:
        std::pair<std::string, DataType> attr;
    };

    inline std::ostream& operator<<(std::ostream &os, AttrNode & node)
    {      
       return os << node.toString();
    }

} } /* namespace */

#endif 

