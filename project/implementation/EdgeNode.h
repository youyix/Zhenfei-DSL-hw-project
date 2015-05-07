#ifndef __EDGENODE_H__
#define __EDGENODE_H__ 1

#include <string>
#include <vector>
#include <iostream> 
#include <sstream> 
#include <unordered_map>    
// #include <lexical_cast>
// #include <boost/lexical_cast.hpp>

#include "VertexNode.h"


namespace intergdb { namespace lang
{
    

    class EdgeNode {
    public:
        EdgeNode(std::string v, std::string u, std::string ts, std::vector<std::string> attrs)
            : v_(v), u_(u), ts_(ts), attrs_(attrs){};

        std::string toString();

        std::string translate();
    private:
        std::string v_;
        std::string u_;
        std::string ts_;
        std::vector<std::string> attrs_;
        
    };

    inline std::ostream& operator<<(std::ostream &os, EdgeNode & node)
    {      
       return os << node.toString();
    }

} } /* namespace */


#endif