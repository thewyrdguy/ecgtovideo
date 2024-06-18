= Convert raw ECG signal into a sequence of video frames

This program takes a text file containing ECG samples, one sample per line,
from stdin, and produces a stream of PNG images to stdout. The latter can
be fed to `ffmpeg ... -f image2pipe ...` to produce encoded video stream.

Each line of the input file must have four whitespace-separated fields:

1. Fractional number representing the timestamp in seconds,
2. Fractional number representing the reading in millivolts,
3. Integer 0 or 1; value of 1 indicates that this sample is
   the position of a QRS (one such indicator per QRS).
4. Integer 0 or 1; value if 1 represents the fact that the sample is
   "anomalous", as marked by the ECG recorder. It is not used by
   this program.

This text format matches the format produced by `illforce` program with
`-T` parameter; it should be easy to produce from any other source.

Despite the presence of timestamps in the input data, the program requires
sample rate to be specified. Default value is 150 samples per second,
which is the sampling rate of popular HealForce one-lead recorders.
Current limitation of the program is that it produces exactly one pixel
of width per sample. Window size can be specified in seconds, and that
value defines both the duration represented in a frame and the picture
width in pixels. Default window is 3 seconds, or 450 pixels at 150 sps.

Picture height is defined by maximum representable voltage given as
a parameter, to achieve standard ratio of width vs. vertical deflection
of the curve representing the input voltage. This ratio had to be tweaked
though, as HealForce recorder seems to output halved mV values. Default
limit is +/- 1 mV.

Because the output of the program is just a stream of concataneted frame
images, without timing information, you need to give input framerate
to the encoder that matches framerate specified to this program.
Default is 25 FPS.

The program is designed as a pipeline in order to make it possible
to use it for visualizing real time ECG signal and produce video stream
for real-time observation. There is no existing tooling for that though,
the hope is to use "IEEE 11073-10406 Basic Electrocardiograph (ECG)
device specialization class attributes" standard to collect acquire
realtime data over Bluetooth low energy from capable devices.

ref: https://www.bluetooth.com/wp-content/uploads/2019/03/PHD\_Transcoding\_WP\_v16.pdf
