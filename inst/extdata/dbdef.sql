CREATE TABLE IF NOT EXISTS Study
(
study_id TEXT NOT NULL,
data_format TEXT,
PRIMARY KEY (study_id)
);

CREATE TABLE IF NOT EXISTS Participant
(
participant_id TEXT NOT NULL,
study_id TEXT NOT NULL,
PRIMARY KEY (participant_id),
FOREIGN KEY (study_id) REFERENCES Study(study_id)
);

CREATE TABLE IF NOT EXISTS ProcessedFiles
(
file_name TEXT NOT NULL,
participant_id TEXT NOT NULL,
study_id TEXT NOT NULL,
PRIMARY KEY (file_name),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id),
FOREIGN KEY (study_id) REFERENCES Study(study_id)
);

CREATE TABLE IF NOT EXISTS Accelerometer
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
end_time TEXT,
n INTEGER,
x_mean REAL,
y_mean REAL,
z_mean REAL,
x_median REAL,
y_median REAL,
z_median REAL,
x_std REAL,
y_std REAL,
z_std REAL,
x_aad REAL,
y_aad REAL,
z_aad REAL,
x_min REAL,
y_min REAL,
z_min REAL,
x_max REAL,
y_max REAL,
z_max REAL,
x_max_min_diff REAL,
y_max_min_diff REAL,
z_max_min_diff REAL,
x_mad REAL,
y_mad REAL,
z_mad REAL,
x_iqr REAL,
y_iqr REAL,
z_iqr REAL,
x_neg_n INTEGER,
y_neg_n INTEGER,
z_neg_n INTEGER,
x_pos_n INTEGER,
y_pos_n INTEGER,
z_pos_n INTEGER,
x_above_mean INTEGER,
y_above_mean INTEGER,
z_above_mean INTEGER,
x_energy REAL,
y_energy REAL,
z_energy REAL,
avg_res_acc REAL,
sma REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Activity
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
confidence INTEGER,
type TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS AirQuality
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
air_quality_index INTEGER,
air_quality_level TEXT,
source TEXT,
place TEXT,
latitude TEXT,
longitude TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS AppUsage
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
end_time TEXT,
"start" TEXT,
"end" TEXT,
usage INTEGER,
app TEXT,
package_name TEXT,
last_foreground TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Battery
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
battery_level INTEGER,
battery_status TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Bluetooth
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
start_scan TEXT,
end_scan TEXT,
advertisement_name TEXT,
bluetooth_device_id TEXT,
bluetooth_device_name TEXT,
bluetooth_device_type TEXT,
connectable BOOLEAN,
rssi INTEGER,
tx_power_level INTEGER,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Calendar
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
event_id TEXT,
calendar_id TEXT,
title TEXT,
description TEXT,
"start" TEXT,
"end" TEXT,
all_day BOOLEAN,
location TEXT,
attendees TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Connectivity
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
connectivity_status TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Device
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
device_id TEXT,
hardware TEXT,
device_name TEXT,
device_manufacturer TEXT,
device_model TEXT,
operating_system TEXT,
platform TEXT,
operating_system_version TEXT,
sdk TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Error
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
message TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Geofence
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
center REAL,
dwell INTEGER,
name TEXT,
radius REAL,
state TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Gyroscope
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
x REAL,
y REAL,
z REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Heartbeat
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
period INTEGER,
device_type TEXT,
device_role_name TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS InstalledApps
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
app TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Keyboard
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
body TEXT,
"end" TEXT,
"start" TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Light
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
end_time TEXT,
mean_lux REAL,
std_lux REAL,
min_lux REAL,
max_lux REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Location
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
latitude TEXT,
longitude TEXT,
altitude REAL,
accuracy REAL,
vertical_accuracy REAL,
speed REAL,
speed_accuracy REAL,
heading REAL,
heading_accuracy REAL,
is_mock BOOLEAN,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Memory
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
free_physical_memory DOUBLE,
free_virtual_memory DOUBLE,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Mobility
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
number_of_places INTEGER,
location_variance REAL,
entropy REAL,
normalized_entropy REAL,
home_stay REAL,
distance_travelled double,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Noise
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
end_time TEXT,
mean_decibel REAL,
std_decibel REAL,
min_decibel REAL,
max_decibel REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Pedometer
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
step_count INTEGER,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS PhoneLog
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
call_type TEXT,
datetime TEXT,
duration INTEGER,
formatted_number TEXT,
name TEXT,
number TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Screen
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
screen_event TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS TextMessage
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
address TEXT,
body TEXT,
text_date TEXT,
date_sent TEXT,
is_read INTEGER,
kind TEXT,
size INTEGER,
state TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Timezone
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
timezone TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Weather
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
country TEXT,
area_name TEXT,
weather_main TEXT,
weather_description TEXT,
sunrise TEXT,
sunset TEXT,
latitude TEXT,
longitude TEXT,
pressure INTEGER,
wind_speed REAL,
wind_degree REAL,
humidity INTEGER,
cloudiness INTEGER,
rain_last_hour REAL,
rain_last_3hours REAL,
snow_last_hour REAL,
snow_last_3hours REAL,
temperature REAL,
temp_min REAL,
temp_max REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Wifi
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
ssid TEXT,
bssid TEXT,
ip TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id)
);
