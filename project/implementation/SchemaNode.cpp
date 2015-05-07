#include "SchemaNode.h"

using namespace intergdb::lang;

// char const * SchemaNode::dataTypesStrings[] = { "INT64", "DOUBLE", "STRING" };

// SchemaNode::SchemaNode(std::string *name, DataType* type)
// {

//     attr.first = *name;
//     attr.second = *type;

// }

void
SchemaNode::addAttr(AttrNode &attr)
{
    attributes_.push_back(attr);
}

SchemaNode::SchemaNode(std::vector<AttrNode> &v)
{   
    attributes_ = v;
}

std::string
SchemaNode::toString()
{
    std::stringstream ss;
    ss << "SchemaNode(" << attributes_.size() << ")   ";
    for ( auto a: attributes_ ) {
        ss << a << " ";
    }
    return ss.str();
}

std::string 
SchemaNode::translate()
{   
    
    std::string l = "{";
    std::string r = "}";
    std::string comma = ", ";

    std::string str = l;
    for ( int i=0; i<attributes_.size(); i++ ) {
        str = str + attributes_[i].translate();
        if ( i < attributes_.size() -1 ) {
            str = str + comma;
        } 
    }

    str = str + r;
    return str;
}

// SchemaNode::SchemaNode(std::vector<AttrNode> *v)
// {   
//     attributes_ = *v;
// }

// SchemaNode::SchemaNode(std::vector<AttrNode> v)
// {
//     attributes_ = v;
// }