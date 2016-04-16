using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using QuickGraph;
using static QuickGraph.DotParserAdapter;

namespace MainForm
{
    public partial class Form1 : Form
    {
        public Form1()
        {
            InitializeComponent();
        }

        private void ParseButtonClick(object sender, EventArgs e)
        {
            try
            {
                var dot = Program.Editor.Text;
                var vertexFun = VertexFunctions.WeightOrFallback(0);
                var edgeFun = EdgeFunctions<KeyValuePair<string, int>>.VerticesOnly;

                var graph = BidirectionalGraph<KeyValuePair<string, int>, SEdge<KeyValuePair<string, int>>>.LoadDot(dot, vertexFun, edgeFun);

                var totalWeight = graph.Vertices.Sum(vertex => vertex.Value);
                MessageBox.Show($"{graph.VertexCount} vertice(s), {graph.EdgeCount} edge(s). Total vertices weight: {totalWeight}");
            }
            catch (Exception ex)
            {
                MessageBox.Show(ex.Message);
            }
        }

        private void algorithmsList_SelectedIndexChanged(object sender, EventArgs e)
        {
            //codeEditorPanel.Controls.Clear();
            playerPanel.Controls.Clear();

            var algorithm = Program.Algorithms[algorithmsList.Text];

            //codeEditorPanel.Controls.Add(algorithm.Input);
            authorLabel.Text = algorithm.Author;
            descriptionLabel.Text = algorithm.Description;
        }

        private async void openFileButton_Click(object sender, EventArgs e)
        {
            var dialog = new OpenFileDialog
            {
                Filter = "Text and DOT files|*.txt;*.dot",
                Multiselect = false
            };

            if (dialog.ShowDialog() != DialogResult.OK) return;
            try
            {
                Program.Editor.Text = await new StreamReader(dialog.FileName).ReadToEndAsync();
            }
            catch (Exception)
            {
                MessageBox.Show("Could not read the file.");
            }
        }

        private void newButton_Click(object sender, EventArgs e)
        {
            Program.Editor.Clear();
        }
    }
}