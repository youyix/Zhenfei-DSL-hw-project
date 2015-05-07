#include "VertexNode.h"


using namespace intergdb::lang;

std::string
VertexNode::toString()
{
    // std::stringstream ss;
    // ss << "SchemaNode(" << attributes_.size() << ")   ";
    // for ( auto a: attributes_ ) {
    //     ss << a << " ";
    // }
    return "";
}


std::string 
VertexNode::translate()
{   
    std::string l_paran = "(";
    std::string r_paran = ")";
    std::string comma = ", ";

    std::string str = l_paran
                    + id_ + ", ";
                 
    for ( int i=0; i<attrs_.size(); i++ ) {
        str = str + attrs_[i];
        if ( i < attrs_.size()-1  ) {
            str += comma;
        }
        
    }
    str = str + r_paran;
    return str;
}