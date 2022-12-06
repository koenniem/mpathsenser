CREATE TABLE IF NOT EXISTS study
(
study_id VARCHAR(255) NOT NULL,
data_format VARCHAR(255),
PRIMARY KEY (study_id)
);

CREATE TABLE IF NOT EXISTS participant
(
participant_id VARCHAR(255) NOT NULL,
study_id VARCHAR(255) NOT NULL,
PRIMARY KEY (participant_id),
FOREIGN KEY (study_id) REFERENCES study(study_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS processedfiles
(
file_name VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
study_id VARCHAR(255) NOT NULL,
PRIMARY KEY (file_name),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE,
FOREIGN KEY (study_id) REFERENCES study(study_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS accelerometer
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
x DOUBLE PRECISION,
y DOUBLE PRECISION,
z DOUBLE PRECISION,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS activity
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
confidence INTEGER,
type VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS airquality
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
air_quality_index INTEGER,
air_quality_level VARCHAR(255),
"source" VARCHAR(255),
place VARCHAR(255),
latitude VARCHAR(255),
longitude VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS appusage
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
"start" VARCHAR(255),
"end" VARCHAR(255),
"usage" INTEGER,
app VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS battery
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
battery_level INTEGER,
battery_status VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS bluetooth
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
advertisement_name VARCHAR(255),
bluetooth_device_id VARCHAR(255),
bluetooth_device_name VARCHAR(255),
bluetooth_device_type VARCHAR(255),
connectable INTEGER,
rssi INTEGER,
tx_power_level INTEGER,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS calendar
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
event_id VARCHAR(255),
calendar_id VARCHAR(255),
title VARCHAR(255),
description VARCHAR(255),
"start" VARCHAR(255),
"end" VARCHAR(255),
all_day INTEGER,
"location" VARCHAR(255),
attendees TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS connectivity
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
connectivity_status VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS device
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
device_id VARCHAR(255),
hardware VARCHAR(255),
device_name VARCHAR(255),
device_manufacturer VARCHAR(255),
device_model VARCHAR(255),
operating_system VARCHAR(255),
platform VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS error
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
message TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS geofence
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
center DOUBLE PRECISION,
dwell INTEGER,
name VARCHAR(255),
radius DOUBLE PRECISION,
state VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS gyroscope
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
x DOUBLE PRECISION,
y DOUBLE PRECISION,
z DOUBLE PRECISION,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS installedapps
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
app VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS keyboard
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
body TEXT,
"start" VARCHAR(255),
"end" VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS light
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
mean_lux DOUBLE PRECISION,
std_lux DOUBLE PRECISION,
min_lux DOUBLE PRECISION,
max_lux DOUBLE PRECISION,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS location
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
latitude VARCHAR(255),
longitude VARCHAR(255),
altitude DOUBLE PRECISION,
accuracy DOUBLE PRECISION,
speed DOUBLE PRECISION,
speed_accuracy DOUBLE PRECISION,
heading DOUBLE PRECISION,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS memory
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
free_physical_memory DOUBLE PRECISION,
free_virtual_memory DOUBLE PRECISION,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS mobility
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
number_of_places INTEGER,
location_variance DOUBLE PRECISION,
entropy DOUBLE PRECISION,
normalized_entropy DOUBLE PRECISION,
home_stay DOUBLE PRECISION,
distance_travelled DOUBLE PRECISION,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS noise
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
mean_decibel DOUBLE PRECISION,
std_decibel DOUBLE PRECISION,
min_decibel DOUBLE PRECISION,
max_decibel DOUBLE PRECISION,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS pedometer
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
step_count INTEGER,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS phonelog
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
call_type VARCHAR(255),
datetime VARCHAR(255),
duration INTEGER,
formatted_number VARCHAR(255),
name VARCHAR(255),
number VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS screen
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
screen_event VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS textmessage
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
address VARCHAR(255),
body VARCHAR(255),
text_date VARCHAR(255),
date_sent VARCHAR(255),
is_read INTEGER,
kind VARCHAR(255),
size INTEGER,
state VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS weather
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
country VARCHAR(255),
area_name VARCHAR(255),
weather_main VARCHAR(255),
weather_description VARCHAR(255),
sunrise VARCHAR(255),
sunset VARCHAR(255),
latitude VARCHAR(255),
longitude VARCHAR(255),
pressure INTEGER,
wind_speed DOUBLE PRECISION,
wind_degree DOUBLE PRECISION,
humidity INTEGER,
cloudiness INTEGER,
rain_last_hour DOUBLE PRECISION,
rain_last_3hours DOUBLE PRECISION,
snow_last_hour DOUBLE PRECISION,
snow_last_3hours DOUBLE PRECISION,
temperature DOUBLE PRECISION,
temp_min DOUBLE PRECISION,
temp_max DOUBLE PRECISION,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS wifi
(
measurement_id VARCHAR(255) NOT NULL,
participant_id VARCHAR(255) NOT NULL,
date VARCHAR(255) NOT NULL,
time VARCHAR(255) NOT NULL,
ssid VARCHAR(255),
bssid VARCHAR(255),
ip VARCHAR(255),
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES participant(participant_id) ON DELETE CASCADE
);
