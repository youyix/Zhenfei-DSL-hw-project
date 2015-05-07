//
//DSL - Graph Query Language
//Zhenfei Nie

#include <intergdb/core/InteractionGraph.h>
#include <intergdb/core/Schema.h>
#include <cstdlib>
#include <iostream>
#include <boost/filesystem.hpp>
using namespace std;
using namespace intergdb::core;

int main() {
    Conf test("test", "./hello", {{"name", Schema::STRING}, {"age", Schema::INT64}}, 
        {{"relation", Schema::STRING}});
    bool newDB = !boost::filesystem::exists(test.getStorageDir());
    boost::filesystem::create_directories(test.getStorageDir());
    InteractionGraph graph(test);
    if (newDB) {
        graph.createVertex(5, "ghouan", 23LL);
        graph.createVertex(6, "alice", 43LL);
        graph.addEdge(5, 6, 3.5, "father");
        graph.flush();
    }

    return EXIT_SUCCESS;
}

//Recongnized a edge_query. 
