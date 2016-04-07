using System;
using System.IO;
using System.Linq;
using System.Windows.Forms;
using QuickGraph;

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
            //Program.CurrentAlgorithm.Run(DotLangParser.parse(Program.Editor.Text));
            //Program.CurrentAlgorithm.Run(); // todo: send graph parsed from dot

            var graph = DotLangParser.parse(Program.Editor.Text);
            MessageBox.Show(graph.Vertices.Aggregate("", (current, vertex) => current + vertex + "\n"));
            //MessageBox.Show(graph.GetNodes().Aggregate("", (current, node) => current + node.Item1 + "\n"));
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