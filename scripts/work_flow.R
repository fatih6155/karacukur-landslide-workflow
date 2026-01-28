# Required package
if (!require("DiagrammeR")) install.packages("DiagrammeR")
library(DiagrammeR)

grViz("
digraph G {
  rankdir=LR;
  nodesep=0.7;
  ranksep=0.9;
  fontname='Arial';
  pad='0.6';

  # General settings
  node [shape=box, style='rounded,filled', fontname='Arial', 
        width=4.5, height=1.5, fixedsize=false, fontsize=30, margin='0.3,0.2'];
  
  # Edge properties - arrow labels 25 pt
  edge [fontname='Arial Italic', fontsize=25, color='#2F4F4F', penwidth=3.0];

  # --- MAIN BLOCKS ---
  
  B1 [label=<
    <font point-size='35'><b>Conditioning Factors</b></font><br align='center'/>
    <font point-size='30'>
    <br/>
    ??? Lithology: Fractured monzogranite<br align='left'/>
    ??? Morphology: Steep slopes (&gt;50??) &amp; south-facing aspect<br align='left'/>
    ??? Anthropogenic: Road-cut excavations<br align='left'/>
    </font>
  >, fillcolor='#D9E8FF'];

  B2 [label=<
    <font point-size='35'><b>Progressive Degradation</b></font><br align='center'/>
    <font point-size='30'>
    <br/>
    ??? Thermal fatigue: Cumulative freeze???thaw cycles<br align='left'/>
    ??? Crack propagation: Joint dilation &amp; fatigue<br align='left'/>
    ??? Transition: ???Thaw onset??? (March 8)<br align='left'/>
    </font>
  >, fillcolor='#FFF1CC'];

  B3 [label=<
    <font point-size='35'><b>Immediate Triggers</b></font><br align='center'/>
    <font point-size='30'>
    <br/>
    ??? Hydraulic loading: Snowmelt + peak rainfall<br align='left'/>
    ??? Dynamic stress: Traffic-induced vibrations<br align='left'/>
    ??? Key lag: 14-day hydrological lag<br align='left'/>
    </font>
  >, fillcolor='#FFD9B3'];

  B4 [label=<
    <font point-size='35'><b>Failure Mechanism</b></font><br align='center'/>
    <font point-size='30'>
    <br/>
    ??? Initiation: Wedge sliding (Set 2 &#8745; Set 3)<br align='left'/>
    ??? Evolution: Composite rockfall &amp; toppling<br align='left'/>
    ??? Outcome: Karacukur Landslide (April 22)<br align='left'/>
    </font>
  >, fillcolor='#FFCCCC'];

  # --- FLOW CONNECTIONS ---
  B1 -> B2 [label=' progressive\\nweakening'];
  B2 -> B3 [label=' reduced rock\\nmass strength'];
  B3 -> B4 [label=' threshold\\nexceedance'];

  # --- TIMELINE (Bottom) ---
  node [width=3.5, height=0.8, fontsize=35, style='filled,dashed', color='#666666'];
  
  T1 [label='Long-term', fillcolor='#D9E8FF'];
  T2 [label='Mid-term', fillcolor='#FFF1CC'];
  T3 [label='Short-term', fillcolor='#FFD9B3'];
  T4 [label='Final Event', fillcolor='#FFCCCC'];

  # Alignment
  {rank=same; B1; T1;}
  {rank=same; B2; T2;}
  {rank=same; B3; T3;}
  {rank=same; B4; T4;}

  # Invisible links
  B1 -> T1 [style=invis];
  B2 -> T2 [style=invis];
  B3 -> T3 [style=invis];
  B4 -> T4 [style=invis];
  
  # Time scale labels
  T1 -> T2 [label='Time Scale', fontcolor='#666666', fontsize=30];
  T2 -> T3 [label='Time Scale', fontcolor='#666666', fontsize=30];
  T3 -> T4 [label='Time Scale', fontcolor='#666666', fontsize=30];
}
")
