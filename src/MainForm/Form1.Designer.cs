namespace MainForm
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.button4 = new System.Windows.Forms.Button();
            this.button5 = new System.Windows.Forms.Button();
            this.parseButton = new System.Windows.Forms.Button();
            this.playerPanel = new System.Windows.Forms.GroupBox();
            this.algorithmsList = new System.Windows.Forms.ComboBox();
            this.authorLabel = new System.Windows.Forms.Label();
            this.descriptionLabel = new System.Windows.Forms.Label();
            this.algorithmPanel = new System.Windows.Forms.GroupBox();
            this.editorPanel = new System.Windows.Forms.GroupBox();
            this.editorHost = new System.Windows.Forms.Integration.ElementHost();
            this.openFileButton = new System.Windows.Forms.Button();
            this.newButton = new System.Windows.Forms.Button();
            this.button2 = new System.Windows.Forms.Button();
            this.testLabel = new System.Windows.Forms.Label();
            this.playerPanel.SuspendLayout();
            this.algorithmPanel.SuspendLayout();
            this.editorPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // button4
            // 
            this.button4.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.button4.Location = new System.Drawing.Point(653, 553);
            this.button4.Name = "button4";
            this.button4.Size = new System.Drawing.Size(82, 24);
            this.button4.TabIndex = 5;
            this.button4.Text = "Previous";
            this.button4.UseVisualStyleBackColor = true;
            // 
            // button5
            // 
            this.button5.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.button5.Location = new System.Drawing.Point(741, 553);
            this.button5.Name = "button5";
            this.button5.Size = new System.Drawing.Size(82, 24);
            this.button5.TabIndex = 6;
            this.button5.Text = "Next";
            this.button5.UseVisualStyleBackColor = true;
            // 
            // parseButton
            // 
            this.parseButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.parseButton.AutoSize = true;
            this.parseButton.Location = new System.Drawing.Point(269, 553);
            this.parseButton.Name = "parseButton";
            this.parseButton.Size = new System.Drawing.Size(88, 23);
            this.parseButton.TabIndex = 9;
            this.parseButton.Text = "Parse and Run";
            this.parseButton.UseVisualStyleBackColor = true;
            this.parseButton.Click += new System.EventHandler(this.ParseButtonClick);
            // 
            // playerPanel
            // 
            this.playerPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.playerPanel.AutoSize = true;
            this.playerPanel.Controls.Add(this.testLabel);
            this.playerPanel.Location = new System.Drawing.Point(269, 105);
            this.playerPanel.Name = "playerPanel";
            this.playerPanel.Size = new System.Drawing.Size(554, 442);
            this.playerPanel.TabIndex = 12;
            this.playerPanel.TabStop = false;
            this.playerPanel.Text = "Playback";
            // 
            // algorithmsList
            // 
            this.algorithmsList.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.algorithmsList.FormattingEnabled = true;
            this.algorithmsList.Location = new System.Drawing.Point(9, 21);
            this.algorithmsList.Name = "algorithmsList";
            this.algorithmsList.Size = new System.Drawing.Size(181, 21);
            this.algorithmsList.Sorted = true;
            this.algorithmsList.TabIndex = 13;
            this.algorithmsList.SelectedIndexChanged += new System.EventHandler(this.algorithmsList_SelectedIndexChanged);
            // 
            // authorLabel
            // 
            this.authorLabel.AutoSize = true;
            this.authorLabel.Location = new System.Drawing.Point(196, 16);
            this.authorLabel.Name = "authorLabel";
            this.authorLabel.Size = new System.Drawing.Size(38, 13);
            this.authorLabel.TabIndex = 15;
            this.authorLabel.Text = "Author";
            // 
            // descriptionLabel
            // 
            this.descriptionLabel.AutoSize = true;
            this.descriptionLabel.Location = new System.Drawing.Point(196, 30);
            this.descriptionLabel.Name = "descriptionLabel";
            this.descriptionLabel.Size = new System.Drawing.Size(60, 13);
            this.descriptionLabel.TabIndex = 12;
            this.descriptionLabel.Text = "Description";
            // 
            // algorithmPanel
            // 
            this.algorithmPanel.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.algorithmPanel.AutoSize = true;
            this.algorithmPanel.Controls.Add(this.descriptionLabel);
            this.algorithmPanel.Controls.Add(this.authorLabel);
            this.algorithmPanel.Controls.Add(this.algorithmsList);
            this.algorithmPanel.Location = new System.Drawing.Point(269, 12);
            this.algorithmPanel.Name = "algorithmPanel";
            this.algorithmPanel.Size = new System.Drawing.Size(554, 87);
            this.algorithmPanel.TabIndex = 13;
            this.algorithmPanel.TabStop = false;
            this.algorithmPanel.Text = "Algorithm";
            // 
            // editorPanel
            // 
            this.editorPanel.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left)));
            this.editorPanel.AutoSize = true;
            this.editorPanel.Controls.Add(this.editorHost);
            this.editorPanel.Controls.Add(this.openFileButton);
            this.editorPanel.Controls.Add(this.newButton);
            this.editorPanel.Controls.Add(this.button2);
            this.editorPanel.Location = new System.Drawing.Point(12, 12);
            this.editorPanel.Name = "editorPanel";
            this.editorPanel.Size = new System.Drawing.Size(253, 564);
            this.editorPanel.TabIndex = 14;
            this.editorPanel.TabStop = false;
            this.editorPanel.Text = "Graph";
            // 
            // editorHost
            // 
            this.editorHost.Location = new System.Drawing.Point(6, 48);
            this.editorHost.Name = "editorHost";
            this.editorHost.Size = new System.Drawing.Size(241, 495);
            this.editorHost.TabIndex = 9;
            this.editorHost.Text = "elementHost1";
            this.editorHost.Child = null;
            // 
            // openFileButton
            // 
            this.openFileButton.Location = new System.Drawing.Point(87, 19);
            this.openFileButton.Name = "openFileButton";
            this.openFileButton.Size = new System.Drawing.Size(75, 23);
            this.openFileButton.TabIndex = 8;
            this.openFileButton.Text = "Open";
            this.openFileButton.UseVisualStyleBackColor = true;
            this.openFileButton.Click += new System.EventHandler(this.openFileButton_Click);
            // 
            // newButton
            // 
            this.newButton.Location = new System.Drawing.Point(6, 19);
            this.newButton.Name = "newButton";
            this.newButton.Size = new System.Drawing.Size(75, 23);
            this.newButton.TabIndex = 5;
            this.newButton.Text = "New";
            this.newButton.UseVisualStyleBackColor = true;
            this.newButton.Click += new System.EventHandler(this.newButton_Click);
            // 
            // button2
            // 
            this.button2.Location = new System.Drawing.Point(168, 19);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(75, 23);
            this.button2.TabIndex = 7;
            this.button2.Text = "Save";
            this.button2.UseVisualStyleBackColor = true;
            // 
            // testLabel
            // 
            this.testLabel.AutoSize = true;
            this.testLabel.Location = new System.Drawing.Point(6, 16);
            this.testLabel.Name = "testLabel";
            this.testLabel.Size = new System.Drawing.Size(35, 13);
            this.testLabel.TabIndex = 0;
            this.testLabel.Text = "label1";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(835, 588);
            this.Controls.Add(this.editorPanel);
            this.Controls.Add(this.algorithmPanel);
            this.Controls.Add(this.playerPanel);
            this.Controls.Add(this.button5);
            this.Controls.Add(this.button4);
            this.Controls.Add(this.parseButton);
            this.Name = "Form1";
            this.Text = "Graph Algorithms";
            this.playerPanel.ResumeLayout(false);
            this.playerPanel.PerformLayout();
            this.algorithmPanel.ResumeLayout(false);
            this.algorithmPanel.PerformLayout();
            this.editorPanel.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion
        private System.Windows.Forms.Button button4;
        private System.Windows.Forms.Button button5;
        private System.Windows.Forms.Button parseButton;
        private System.Windows.Forms.GroupBox playerPanel;
        internal System.Windows.Forms.ComboBox algorithmsList;
        private System.Windows.Forms.Label authorLabel;
        private System.Windows.Forms.Label descriptionLabel;
        private System.Windows.Forms.GroupBox algorithmPanel;
        private System.Windows.Forms.GroupBox editorPanel;
        private System.Windows.Forms.Button newButton;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Button openFileButton;
        internal System.Windows.Forms.Integration.ElementHost editorHost;
        private System.Windows.Forms.Label testLabel;
    }
}

