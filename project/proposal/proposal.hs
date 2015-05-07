-- Zhenfei Nie

The problem :
I am going to follow the Prof. Robert's idea of designing a graph query language.


The challenges of course are the implementaion of the querying language. Compared to the 
graph definition language, the querying language is more challenging.

I need to know follow things. The first one is the related topics the existed graph databases.
I would like to know how they are used and worked. The second is that I want to know how to 
design a good querying language.

As for the milestones, the first one should be the implementaion of the language definition, which is
clear now. The second should be the designing of the querying language, which is the most vital part.


For your convenience, there is the language details from the the link: 
    http://www.cs.bilkent.edu.tr/~bgedik/coursewiki/doku.php/cs315:fall2014:project_part1#dokuwiki__top.

1. The graph definition language should support:
    Defining directed graphs
    Defining undirected graphs
    Defining vertex properties
        A vertex property is some data value associated with a vertex.
        It should be possible to attach multiple properties to a vertex.
        A property should be a (name, value) pair. For instance, if a vertex 
        represents a student, then a vertex property could be (“id”, 19945656). 
        Remember that there could be multiple such properties, such as: 
        (“id”, 19945656), (“name”, “Ali Veli”), (“age”, 20).
    Defining edge properties
        It should be possible to attach multiple properties to an edge.
    A dynamic type system for the property values (property names should be strings)
        Support strings, integers, and floats as primitive types. For instance, in 
        the earlier example, the “id” attribute was using an integer value, whereas 
        the “name” attribute was using a “string” value.
        Support lists, sets, and maps as collection types. For instance, it should be 
        possible to have a property like: (“grades”, {“CS315”: 85, “CS101”: 90, “CS666”: 15}). 
        This would be an example of a property, whose value is a map from strings to integers. 
        Arbitrary nesting should be possible as well. For instance, we can have (“grades”, 
            {“CS315”: [85, 80], “CS101”: [90, 98], “CS666”: [15, 3]}). This is an example where 
            the value type is a map from a string to a list of integers.
2. The graph querying language should support:
    Creating regular path queries. A regular path query is a regular expression specifying a path. 
    A path is a series of edges. Importantly, we are not asking you to evaluate path queries. 
    We are asking you to create a language to express them. A path query should be able to specify:
    Concatenation, alternation, and repetition.
    Filters as Boolean expressions, which are composed of predicates defined over edge properties as 
    well as incident vertex properties. For instance, one may want to find all paths of length three 
    (a path of three edges), where the start vertex of the first edge has a vertex property name=“CS”, 
    the second edge has an edge property code=“315”, and finally the third edge's end vertex has a vertex 
    property kind=“rulez!”.
    Support for existence predicates as well as arithmetic expressions and functions in predicate expressions. 
    For instance: an edge containing or not containing a property with a given name or value; or an edge that 
    has a certain property whose value is greater than a constant threshold; or an edge that has a certain 
    property whose value is a string that starts with “A” (this would require a string indexing function).
    Support having variables in path expressions. For instance, you may want to query all paths of length 
    two where both edges in the path have a property called name with the same property value, but the value 
    is not known. In this case, that value becomes a variable.
    Support modularity, that is dividing regular path queries into multiple pieces that are specified separately. 
    This would require giving names to each piece and being able to use those names in a higher-level query.