using Microsoft.VisualStudio.TestTools.UnitTesting;
using DotParserProject;

namespace QuickGraph.DotParserTest
{
    [TestClass]
    public class DotParserTest
    {
        private static void TestIsDirectedEmpty(string s, bool isDirected)
        {
            var graph = DotParser.parse(s);

            Assert.AreEqual(0, graph.VertexCount);
            Assert.AreEqual(0, graph.EdgeCount);
            Assert.AreEqual(isDirected, graph.IsDirected);
        }

        [TestMethod]
        public void EmptyUndirectedGraph()
        {
            TestIsDirectedEmpty("graph { }", false);
        }

        [TestMethod]
        public void EmptyDirectedGraph()
        {
            TestIsDirectedEmpty("digraph { }", true);
        }

        [TestMethod]
        public void SingleNode()
        {
            var graph = DotParser.parse("graph { a }");

            Assert.AreEqual(1, graph.VertexCount);
            Assert.AreEqual(0, graph.EdgeCount);
            // todo: check vertex name
        }

        [TestMethod]
        public void NumeralNodeId()
        {
            var graph = DotParser.parse("graph { 1 2 }");

            Assert.AreEqual(2, graph.VertexCount);
            Assert.AreEqual(0, graph.EdgeCount);
            // todo: check vertex name
        }

        [TestMethod]
        public void SingleEdge()
        {
            var graph = DotParser.parse("graph { a -- b }");

            Assert.AreEqual(2, graph.VertexCount);
            Assert.AreEqual(1, graph.EdgeCount);
        }

        [TestMethod]
        public void MultipleEdges()
        {
            var graph = DotParser.parse("graph { a -- b c -- d -- e }");

            Assert.AreEqual(5, graph.VertexCount);
            Assert.AreEqual(3, graph.EdgeCount);
        }

        [TestMethod]
        public void MultiEdge()
        {
            var graph = DotParser.parse("graph { a -- b a -- b }");

            Assert.AreEqual(2, graph.VertexCount);
            Assert.AreEqual(2, graph.EdgeCount);
        }

        [TestMethod]
        public void StrictGraph()
        {
            {
                var graph = DotParser.parse("graph { a -- b a -- b }");

                Assert.AreEqual(2, graph.VertexCount);
                Assert.AreEqual(1, graph.EdgeCount);
            }
        }

        [TestMethod]
        public void KeywordLabels()
        {
            var graph = DotParser.parse("graph { \"graph\" -- \"node\" }");

            Assert.AreEqual(2, graph.VertexCount);
            Assert.AreEqual(1, graph.EdgeCount);
            // todo: check name (assert contains keywords)
        }

    //    [TestMethod]
    //    public void TestMethod1()
    //    {
    //        var str = "strict graph t { 6 [label = \"v1\", yo = \"yo;!\" ] \n 1 -- 2 -- 3 \n 2 -- 1 \n 1 -- 1 \n 3 -- 4 \n }";
    //        var g1 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //        Assert.AreEqual(5, g1.EdgeCount);
    //        Assert.AreEqual(5, g1.VertexCount);
    //        Assert.IsTrue(g1.ContainsEdge("1", "2"));
    //        Assert.IsTrue(g1.ContainsEdge("2", "3"));
    //        Assert.IsTrue(g1.ContainsEdge("1", "1"));
    //        Assert.IsTrue(g1.ContainsEdge("3", "4"));
    //        Assert.IsTrue(g1.ContainsVertex("6"));
    //    }

    //    //[TestMethod]
    //    //public void TestMethod2()
    //    //{
    //    //    var adr = "..\\..\\..\\test_inputs\\test2.dot";
    //    //    Func<string, Tuple<string, string>[], string> f = (v, attrs) => v;
    //    //    Func<string, string, Tuple<string, string>[], SUndirectedEdge<string>> f2 = (v1, v2, attrs) => new SUndirectedEdge<string>(v1, v2);
    //    //    var g2 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDotFromFile(adr, f, f2);

    //    //    Assert.AreEqual(4, g2.EdgeCount);
    //    //    Assert.AreEqual(5, g2.VertexCount);
    //    //}

    //    [TestMethod]
    //    public void TestMethod3()
    //    {
    //        var str = "strict graph t { 1 \n 1 -- 1 \n 1 -- 1 \n 1 -- 1; \n 1 -- 1 \n }";
    //        var g1 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //        Assert.AreEqual(4, g1.EdgeCount);
    //        Assert.AreEqual(1, g1.VertexCount);
    //    }

    //    [TestMethod]
    //    public void TestMethod4()
    //    {
    //        var str = "strict graph t { 1 2 \n 3 \n 4 \n 5 \n }";
    //        var g1 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //        Assert.AreEqual(0, g1.EdgeCount);
    //        Assert.AreEqual(5, g1.VertexCount);
    //        Assert.IsFalse(g1.ContainsEdge("1", "2"));
    //        Assert.IsFalse(g1.ContainsEdge("2", "3"));
    //        Assert.IsFalse(g1.ContainsEdge("3", "4"));
    //        Assert.IsFalse(g1.ContainsEdge("4", "5"));
    //    }

    //    [TestMethod]
    //    public void TestMethod5()
    //    {
    //        var str = "strict graph t { 9 \n }";
    //        var g1 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //        Assert.AreEqual(0, g1.EdgeCount);
    //        Assert.AreEqual(1, g1.VertexCount);
    //        Assert.IsTrue(g1.ContainsVertex("9"));
    //        Assert.IsFalse(g1.ContainsEdge("9", "9"));
    //    }
    //    [TestMethod]
    //    public void TestMethod6()
    //    {
    //        var str = "strict graph t { 8 \n  3 -> 6 \n 6 -> 1 \n 9 -> 10; \n }";
    //        var g1 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //        Assert.AreEqual(3, g1.EdgeCount);
    //        Assert.AreEqual(6, g1.VertexCount);
    //        Assert.IsTrue(g1.ContainsVertex("8"));
    //        Assert.IsTrue(g1.ContainsEdge("3", "6"));
    //        Assert.IsTrue(g1.ContainsEdge("6", "1"));
    //        Assert.IsTrue(g1.ContainsEdge("9", "10"));
    //    }

    //    [TestMethod]
    //    public void TestMethod7()
    //    {
    //        var str = "strict graph t { 1 -- 2 -- 3 -- 4 -- 6 -- 7 \n 2 -- 1 \n }";
    //        var g1 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //        Assert.AreEqual(6, g1.EdgeCount);
    //        Assert.AreEqual(6, g1.VertexCount);
    //        Assert.IsTrue(g1.ContainsEdge("1", "2"));
    //        Assert.IsTrue(g1.ContainsEdge("2", "3"));
    //        Assert.IsTrue(g1.ContainsEdge("3", "4"));
    //        Assert.IsTrue(g1.ContainsEdge("4", "6"));
    //        Assert.IsTrue(g1.ContainsEdge("6", "7"));
    //    }
    //    [TestMethod]
    //    public void TestMethod8()
    //    {
    //        var str = "strict digraph \"my graph\" { 1 -- 2 [weight=9] \n 1 -- 2 [weight=10] \n 2 -- 1[weight=11]; \n 2 -- 1[weight=12] \n 1 [label=\"v1\"] \n 2 [label=\"v2\"] \n }";
    //        var g1 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //        Assert.AreEqual(4, g1.EdgeCount);
    //        Assert.AreEqual(2, g1.VertexCount);
    //    }

    //    [TestMethod]
    //    public void Attributes()
    //    {
    //        var str = "graph { 3 \n NodE [color=red] \n 2 [\"node\"=second] \n 1 -- 2 \n edge [color=yellow] \n 2 -- 3 \n edge [size=5] \n node [size=4] \n 1 -- 4 \n }";
    //        var g = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //        Assert.AreEqual(4, g.VertexCount);
    //        Assert.AreEqual(3, g.EdgeCount);
    //    }

    //    //[TestMethod]
    //    //public void MultipleEdges()
    //    //{
    //    //    var str = "strict graph t { 1 -- { 2 3 4 } \n }";
    //    //    Func<string, Tuple<string, string>[], string> f = (v, attrs) => v;
    //    //    Func<string, string, Tuple<string, string>[], SUndirectedEdge<string>> f2 = (v1, v2, attrs) => new SUndirectedEdge<string>(v1, v2);
    //    //    var g1 = BidirectionalGraph<string, SUndirectedEdge<string>>.LoadDot(str, f, f2);

    //    //    //Assert.AreEqual(0, g1.EdgeCount);
    //    //    Assert.AreEqual(4, g1.VertexCount);
    //    //}
    }
}
