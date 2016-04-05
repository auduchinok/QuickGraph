using Common;
using Mono.Addins;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;
using System.Windows.Media;
using ICSharpCode.AvalonEdit;

[assembly: AddinRoot("GraphTasks", "1.0")]
namespace MainForm
{
    internal static class Program
    {
        internal static Dictionary<string, IAlgorithm> Algorithms { get; private set; }
        internal static IAlgorithm CurrentAlgorithm { get; set; }
        internal static TextEditor Editor { get; private set; }

        [STAThread]
        private static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);

            AddinManager.Initialize();
            AddinManager.Registry.Update();
            Algorithms = AddinManager.GetExtensionObjects<IAlgorithm>().ToDictionary(algorithm => algorithm.Name);
            
            var form = new Form1();
            foreach (var algorithm in Algorithms.Values)
            {
                form.algorithmsList.Items.Add(algorithm.Name);
            }

            Editor = new TextEditor {FontFamily = new FontFamily("Consolas")};
            form.editorHost.Child = Editor;

            Application.Run(form);
        }
    }
}
