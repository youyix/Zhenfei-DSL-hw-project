#ifndef __GRAPHNODE_H__
#define __GRAPHNODE_H__ 1

#include <string>
#include <vector>
#include <iostream> 
#include <sstream> 
#include <unordered_map>    

#include "ConfNode.h"
#include "VertexNode.h"
#include "EdgeNode.h"

namespace intergdb { namespace lang
{

    class GraphNode {
    public:

        GraphNode(std::string self_name, ConfNode & conf): 
            s_name_(self_name), conf_(conf){};

        void addVertices(std::vector<VertexNode> v_v);
        void addEdges(std::vector<EdgeNode> e_v);

        std::string translate();

        std::string toString();
    private:
        void tranlate();


        std::string s_name_;
        ConfNode &conf_;

        std::vector<EdgeNode> e_v_;
        std::vector<VertexNode> v_v_;

    };

    inline std::ostream& operator<<(std::ostream &os, GraphNode & node)
    {      
       return os << node.toString();
    }


} } /* namespace */


#endif