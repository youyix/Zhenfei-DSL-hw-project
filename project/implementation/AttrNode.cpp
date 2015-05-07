#include "AttrNode.h"

using namespace intergdb::lang;

// char const * SchemaNode::dataTypesStrings[] = { "INT64", "DOUBLE", "STRING" };


AttrNode::AttrNode(std::string name, DataType type)
{

    attr.first = name;
    attr.second = type;

}


AttrNode::AttrNode(std::string name, std::string type)
{
    // std::cout << "M<NM" <<std::endl;
    
    DataType dt;
    if ( type.compare("STRING") == 0 ) {
        dt = STRING;
    } else if ( type.compare("INT64" ) == 0) {
        dt = INT64;
    } else if ( type.compare("DOUBLE") == 0 ) {
        dt = DOUBLE;
    } else {
        std::cout << "!!!!!" << std::endl;
        exit(-1);
    }

    attr.first = name;
    attr.second = dt;

}

std::string 
AttrNode::toString()
{
    // return attr.first + " " + dataTypesStrings[attr.second];
    return translate();
}

std::string 
AttrNode::translate()
{   
    std::string schema("Schema::");
    std::string name = attr.first;
    std::string type = std::string(dataTypesStrings[attr.second]);
    std::string l = "{";
    std::string r = "}";
    std::string comma = ", ";
    std::string str = l + name + comma + schema + type + r;
    return str;
}