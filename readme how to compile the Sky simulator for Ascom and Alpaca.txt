How to compile the "Sky simulator for Ascom and Alpaca":

1) Install Lazurus  (this will also install Free Pascal Compiler)


2a) Start Lazarus GUI.  Load sky_simulator.lpi (or sky_simulator_linux.lpi or sky_simulator_macos.lpi). Menu Run, Run or Compile

2b) Command line:

  Windows: 
     lazbuild -B sky_simulator.lpi

  Linux:  
    lazbuild -B sky_simulator_linux.lpi

  Mac:  
    lazbuild -B sky_simulator_macos.lpi

To include package Synape, just load ./synapse/laz_synapse.lpk package in the editor of Lazarus.  Then load sky_simulator_linux.lpi  (or sky_simulator_linux.lpi or sky_simulator_macos.lpi) and compile.

