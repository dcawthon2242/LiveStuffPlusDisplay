# LiveStuffPlusDisplay

Idle project, waiting for monitor and Raspberry Pi 5 to continue.

Project hopes to grab trackman portable API data, done through main.py, and parse live trackman data to output a live Stuff+ and live projected pitch outcome in real time. Pitch grade model done in R using lightgbm (D1PitchGrade), and pitch outcomes modeled using a variety of individual binary models, done with D1SwingDecisions. Pitcher handedness and hitter handedness are set with a physical switch placed on the machine.

Working on improving hardware and streamlining software to cut down on time, attempted with Raspberry Pi 3, but delay times were too slow for any type of use within a bullpen setting. Also looking to host onto an ipad or tablet as opposed to a monitor with HDMI.
