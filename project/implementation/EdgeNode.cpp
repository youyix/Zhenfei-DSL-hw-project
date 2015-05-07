#include "EdgeNode.h"


using namespace intergdb::lang;

std::string
EdgeNode::toString()
{
    // std::stringstream ss;
    // ss << "SchemaNode(" << attributes_.size() << ")   ";
    // for ( auto a: attributes_ ) {
    //     ss << a << " ";
    // }
    return "EdgeNode";
}

std::string 
EdgeNode::translate()
{   

    std::string l_paran = "(";
    std::string r_paran = ")";
    std::string comma = ", ";
    std::string str = l_paran
                    + v_ + ", "
                    + u_ + ", "
                    + ts_ + ", "; 
                    // + boost::lexical_cast<std::string>(v_) + ", "
                    // + boost::lexical_cast<std::string>(u_) + ", "
                    // + boost::lexical_cast<std::string>(ts_) + ", "; 

    for ( int i=0; i<attrs_.size(); i++ ) {
        str = str + attrs_[i];
        if ( i < attrs_.size()-1  ) {
            str += comma;
        }       
    }
    str = str + r_paran;
    return str;
}