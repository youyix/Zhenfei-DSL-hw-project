

#include "GraphNode.h"


using namespace intergdb::lang;

std::string
GraphNode::toString()
{
    // std::stringstream ss;
    // ss << "GraphNode(" << attributes_.size() << ")   ";
    // for ( auto a: attributes_ ) {
    //     ss << a << " ";
    // }
    return "";
}


void 
GraphNode::addVertices(std::vector<VertexNode> v_v)
{
    v_v_ = v_v;
}

void 
GraphNode::addEdges(std::vector<EdgeNode> e_v)
{
    e_v_ = e_v;
}

std::string 
GraphNode::translate()
{   

    std::string l = "(";
    std::string r = ")";
    std::string comma = ", ";

    std::string semicolon = ";\n";
    std::string tab = "    ";
    std::string dot = ".";


    std::string createVertex = "createVertex";
    std::string addEdge = "addEdge";


    std::string str("\n//DSL - Graph Query Language\n//Zhenfei Nie\n\n");
    str += std::string("#include <intergdb/core/InteractionGraph.h>\n") 
                        + std::string("#include <intergdb/core/Schema.h>\n")
                        + std::string("#include <cstdlib>\n")
                        + std::string("#include <iostream>\n")
                        + std::string("#include <boost/filesystem.hpp>\n")
                        + std::string("using namespace std;")
                        + std::string("\nusing namespace intergdb::core;\n\n");
    str += std::string("int main() {\n");

    str += tab + conf_.translate() + semicolon;

    str += tab + std::string("bool newDB = !boost::filesystem::exists(") + conf_.getSelfName() + std::string(".getStorageDir());\n");
    str += tab + std::string("boost::filesystem::create_directories(") + conf_.getSelfName() + std::string(".getStorageDir());\n");
    
    str += tab + std::string("InteractionGraph ") + s_name_ + l + conf_.getSelfName() + r + semicolon;

    str += tab + std::string("if (newDB) {\n");

    for ( auto a: v_v_ ) {
        str += tab + tab + s_name_ + dot + createVertex + a.translate() + semicolon;
    }

    for ( auto a: e_v_ ) {
        str += tab + tab + s_name_ + dot + addEdge + a.translate() + semicolon;
    }

    str += tab + tab + s_name_ + dot + std::string("flush();");
    str += std::string("\n") + tab + std::string("}\n");
    str += std::string("\n") + tab + std::string("return EXIT_SUCCESS;\n}\n");
    return str;
}
