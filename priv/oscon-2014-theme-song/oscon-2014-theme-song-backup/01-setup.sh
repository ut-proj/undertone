$ git clone https://gist.github.com/8b4404e538e61c7996a5.git
$ cd 8b4404e538e61c7996a5
$ mkdir salamander && cd salamander
$ curl -L -O https://archive.org/download/SalamanderDrumkit/salamanderDrumkit.tar.bz2
$ curl -L -O https://github.com/johnsen/drumsandpercussion/blob/master/SalamanderKick/salamanderdrum-kick-r1.tar.gz

# Check for the latest here:
# * http://freepats.zenvoid.org/Piano/acoustic-grand-piano.html
$ curl -L -O http://freepats.zenvoid.org/Piano/SalamanderGrandPianoV3_44.1khz16bit.tar.bz2
$ tar xvfz salamanderDrumkit.tar.bz2
$ tar xvfz salamanderdrum-kick-r1.tar.gz
$ mv Kick/kick* OH/
$ tar xvfz SalamanderGrandPianoV3_44.1khz16bit.tar.bz2

$ brew tap benswift/extempore
$ brew install extempore
$ cd /usr/local/Cellar/extempore/0.52
$ ./extempore
