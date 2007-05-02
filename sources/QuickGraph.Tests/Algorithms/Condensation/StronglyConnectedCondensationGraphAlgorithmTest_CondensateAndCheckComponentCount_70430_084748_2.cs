using System.CodeDom.Compiler;
using Microsoft.Pex.Framework;
using Microsoft.Pex.Framework.CodeDom;
using QuickGraph.Unit;

namespace QuickGraph.Algorithms.Condensation
{
    
    
    public partial class StronglyConnectedCondensationGraphAlgorithmTest
    {
        
        /// <summary>
        ///  Test generated by Pex
        ///</summary>
        ///<remarks>
        ///  <para>
        ///    See http://codebox/pex/Wiki/View.aspx?title=Test+Failures for more information.
        ///  </para>
        ///</remarks>
        ///<exception cref="System.ArgumentNullException">
        ///  Value cannot be null.
        ///Parameter name: visitedGraph
        ///</exception>
        [QuickGraph.Unit.TestAttribute()]
        [PexUnexpectedException(typeof(System.ArgumentNullException))]
        [PexGeneratedBy(typeof(QuickGraph.Algorithms.Condensation.StronglyConnectedCondensationGraphAlgorithmTest), "StronglyConnectedCondensationGraphAlgorithmTest.CondensateAndCheckComponentCount(" +
            "IVertexAndEdgeListGraph<String,Edge<String>>)")]
        [GeneratedCode("Pex", "1.1.20429.0")]
        public void CondensateAndCheckComponentCount_IVertexAndEdgeListGraph2_70430_084748_1_01()
        {
            this.CondensateAndCheckComponentCount(((QuickGraph.IVertexAndEdgeListGraph<string, QuickGraph.Edge<string>>)(null)));
        }
    }
}