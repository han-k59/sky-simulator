#!/bin/bash

cd ~/sky_simulator_install
rm ~/sky_simulator.fpc/sky_simulator
/home/h/fpcupdeluxe/lazarus/lazbuild /home/h/sky_simulator.fpc/sky_simulator_linux.lpi
if [[ ! -f ~/sky_simulator.fpc/sky_simulator ]] ; then
    echo 'AMD64 file does not exist, aborting!!'
    exit
fi

cp /home/h/sky_simulator.fpc/sky_simulator /home/h/sky_simulator_install/sky_simulator_amd64/opt/sky_simulator
sudo fakeroot dpkg-deb --build ~/sky_simulator_install/sky_simulator_amd64

# build .deb
sudo dpkg -i ./sky_simulator_amd64.deb
tar -czvf sky_simulator_amd64.tar.gz /opt/sky_simulator/sky_simulator  /opt/sky_simulator/sky_simulator.ico /opt/sky_simulator/copyright.txt /opt/sky_simulator/deep_sky.csv /usr/share/applications/sky_simulator.desktop  /opt/sky_simulator/g*.290


#GTK3 VERSION

cd ~/sky_simulator_install
rm ~/sky_simulator.fpc/sky_simulator
/home/h/fpcupdeluxe_new/fpcupdeluxe/lazarus/lazbuild /home/h/sky_simulator.fpc/sky_simulator_linux_gtk3.lpi
if [[ ! -f ~/sky_simulator.fpc/sky_simulator ]] ; then
    echo 'AMD64 file does not exist, aborting!!'
    exit
fi

cp /home/h/sky_simulator.fpc/sky_simulator /home/h/sky_simulator_install/sky_simulator_amd64_gtk3/opt/sky_simulator
sudo fakeroot dpkg-deb --build ~/sky_simulator_install/sky_simulator_amd64_gtk3

# build .deb
sudo dpkg -i ./sky_simulator_amd64_gtk3.deb
tar -czvf sky_simulator_amd64_gtk3.tar.gz /opt/sky_simulator/sky_simulator  /opt/sky_simulator/sky_simulator.ico /opt/sky_simulator/copyright.txt /opt/sky_simulator/deep_sky.csv /usr/share/applications/sky_simulator.desktop  /opt/sky_simulator/g*.290

read -p "Press any key to continue"








