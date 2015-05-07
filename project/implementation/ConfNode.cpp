#include "ConfNode.h"

using namespace intergdb::lang;


std::string 
ConfNode::translate()
{   
    std::string title = "Conf ";

    std::string l = "(";
    std::string r = ")";
    std::string comma = ", ";
    std::string ret = "\n";
    std::string tab = "    ";


    std::string str = title + s_name_ + l + name_  + comma
                    + db_path_ + comma
                    + vertex_schema_.translate() + comma + ret + tab + tab
                    + edge_schema_.translate()
                    + r;
    
    return str;
}
