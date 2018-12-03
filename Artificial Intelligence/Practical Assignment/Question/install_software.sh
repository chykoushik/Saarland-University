#!/bin/bash

if ! [ `id -u` -eq 0 ] ; then
    echo "Script must be run as root."
    exit 1
fi

ARCHIVE=eclipse-oxygen-griddle.tar.gz

cd /tmp
wget http://fai.cs.uni-saarland.de/ai18/$ARCHIVE
tar -xf $ARCHIVE
mv eclipse /usr/local/lib/
rm -rf $ARCHIVE

install -D /dev/stdin "/usr/local/bin/eclipse" <<END
#!/bin/bash
export ECLIPSE_HOME=/usr/local/lib/eclipse
exec \$ECLIPSE_HOME/eclipse "\$@"
END
 
install -Dm644 /dev/stdin "/usr/share/applications/eclipse.desktop" <<END
[Desktop Entry]
Name=Eclipse
Comment=A Java Development Environment
Icon=eclipse
Exec=eclipse
Terminal=false
Type=Application
Categories=Development;IDE;Java;
StartupNotify=true
END

ln -s /usr/share/applications/eclipse.desktop /home/ai/Desktop/eclipse.desktop
chown -R ai:ai /home/ai/Desktop/eclipse.desktop


for i in 16 32 48 256; do
install -Dm644 /usr/local/lib/eclipse/plugins/org.eclipse.platform_*/eclipse$i.png \
  "/usr/share/icons/hicolor/${i}x$i/apps/eclipse.png"
done
 

ARCHIVE=ggp-base-master.zip

cd /tmp
wget http://fai.cs.uni-saarland.de/ai18/$ARCHIVE
unzip $ARCHIVE
mkdir /home/ai/bin/
mv ggp-base-master /home/ai/bin/
rm -rf $ARCHIVE

chown -R ai:ai /home/ai/bin/
chmod +x /home/ai/bin/ggp-base-master/run_server.sh

install -Dm644 /dev/stdin "/home/ai/Desktop/GGP.desktop" <<END
[Desktop Entry]
Comment[en_US]=
Comment=
Exec=/home/ai/bin/ggp-base-master/run_server.sh
GenericName[en_US]=
GenericName=
Icon=kapman
Name[en_US]=GGP
Name=GGP
Path=/home/ai/bin/ggp-base-master
StartupNotify=true
Terminal=false
TerminalOptions=
Type=Application
END

chown -R ai:ai /home/ai/Desktop/GGP.desktop
