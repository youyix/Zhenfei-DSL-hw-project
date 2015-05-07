#ifndef __VERTEXNODE_H__
#define __VERTEXNODE_H__ 1

#include <string>
#include <vector>
#include <iostream> 
#include <sstream> 
#include <unordered_map>    

// #include <lexical_cast>
// #include <boost/lexical_cast.hpp>

namespace intergdb { namespace lang
{
    typedef uint64_t VertexId;
    typedef double Timestamp;

    class VertexNode {
    public:
        VertexNode(std::string id, std::vector<std::string> attrs)
            :id_(id), attrs_(attrs){};

        std::string translate();

        std::string toString();
    private:
        std::string id_;
        std::vector<std::string> attrs_;
        
    };

    inline std::ostream& operator<<(std::ostream &os, VertexNode & node)
    {      
       return os << node.toString();
    }

} } /* namespace */


#endif