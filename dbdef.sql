PRAGMA foreign_keys = 1;

PRAGMA main.synchronous = 3;

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
FOREIGN KEY (study_id) REFERENCES Study(study_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS ProcessedFiles
(
file_name TEXT NOT NULL,
participant_id TEXT NOT NULL,
study_id TEXT NOT NULL,
PRIMARY KEY (file_name),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE,
FOREIGN KEY (study_id) REFERENCES Study(study_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Accelerometer
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
x REAL,
y REAL,
z REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
latitude BLOB,
longitude BLOB,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS AppUsage
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
start TEXT,
end TEXT,
usage INTEGER,
app TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Bluetooth
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
advertisement_name TEXT,
bluetooth_device_id TEXT,
bluetooth_device_name TEXT,
bluetooth_device_type TEXT,
connectable INTEGER,
rssi INTEGER,
tx_power_level INTEGER,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Calendar
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
event_id INTEGER,
calendar_id INTEGER,
title TEXT,
description TEXT,
start TEXT,
end TEXT,
all_day INTEGER,
location TEXT,
attendees TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Connectivity
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
connectivity_status TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS InstalledApps
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
apps TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Keyboard
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
body TEXT,
end TEXT,
start TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Light
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
mean_lux REAL,
std_lux REAL,
min_lux REAL,
max_lux REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Location
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
latitude BLOB,
longitude BLOB,
altitude REAL,
accuracy REAL,
speed REAL,
speed_accuracy REAL,
heading REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Memory
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
free_physical_memory INTEGER,
free_virtual_memory INTEGER,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Mobility
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
number_of_places INTEGER,
location_variance INTEGER,
entropy REAL,
normalized_entropy REAL,
home_stay REAL,
distance_travelled INTEGER,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Noise
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
mean_decibel REAL,
std_decibel REAL,
min_decibel REAL,
max_decibel REAL,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Pedometer
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
step_count INTEGER,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Screen
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
screen_event TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
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
latitude BLOB,
longitude BLOB,
pressure INTEGER,
wind_speed REAL,
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
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS Wifi
(
measurement_id TEXT NOT NULL,
participant_id TEXT NOT NULL,
date TEXT NOT NULL,
time TEXT NOT NULL,
ssid TEXT,
bssid TEXT,
PRIMARY KEY (measurement_id),
FOREIGN KEY (participant_id) REFERENCES Participant(participant_id) ON DELETE CASCADE
);

CREATE INDEX idx_accelerometer ON Accelerometer(participant_id);

CREATE INDEX idx_airquality ON AirQuality(participant_id);

CREATE INDEX idx_activity ON Activity(participant_id);

CREATE INDEX idx_appusage ON AppUsage(participant_id);

CREATE INDEX idx_battery ON Battery(participant_id);

CREATE INDEX idx_bluetooth ON Bluetooth(participant_id);

CREATE INDEX idx_calendar ON Calendar(participant_id);

CREATE INDEX idx_connectivity ON Connectivity(participant_id);

CREATE INDEX idx_device ON Device(participant_id);

CREATE INDEX idx_geofence ON Geofence(participant_id);

CREATE INDEX idx_gyroscope ON Gyroscope(participant_id);

CREATE INDEX idx_installedapps ON InstalledApps(participant_id);

CREATE INDEX idx_keyboard ON Keyboard(participant_id);

CREATE INDEX idx_light ON Light(participant_id);

CREATE INDEX idx_location ON Location(participant_id);

CREATE INDEX idx_memory ON Memory(participant_id);

CREATE INDEX idx_mobility ON Mobility(participant_id);

CREATE INDEX idx_noise ON Noise(participant_id);

CREATE INDEX idx_pedometer ON Pedometer(participant_id);

CREATE INDEX idx_phonelog ON PhoneLog(participant_id);

CREATE INDEX idx_screen ON Screen(participant_id);

CREATE INDEX idx_textmessage ON TextMessage(participant_id);

CREATE INDEX idx_weather ON Weather(participant_id);

CREATE INDEX idx_wifi ON Wifi(participant_id);

CREATE INDEX date_accelerometer ON Accelerometer(date)

CREATE INDEX date_airquality ON AirQuality(date)

CREATE INDEX date_activity ON Activity(date)

CREATE INDEX date_appusage ON AppUsage(date)

CREATE INDEX date_battery ON Battery(date)

CREATE INDEX date_bluetooth ON Bluetooth(date)

CREATE INDEX date_calendar ON Calendar(date)

CREATE INDEX date_connectivity ON Connectivity(date)

CREATE INDEX date_device ON Device(date)

CREATE INDEX date_geofence ON Geofence(date)

CREATE INDEX date_gyroscope ON Gyroscope(date)

CREATE INDEX date_installedapps ON InstalledApps(date)

CREATE INDEX date_keyboard ON Keyboard(date)

CREATE INDEX date_light ON Light(date)

CREATE INDEX date_location ON Location(date)

CREATE INDEX date_memory ON Memory(date)

CREATE INDEX date_mobility ON Mobility(date)

CREATE INDEX date_noise ON Noise(date)

CREATE INDEX date_pedometer ON Pedometer(date)

CREATE INDEX date_phonelog ON PhoneLog(date)

CREATE INDEX date_screen ON Screen(date)

CREATE INDEX date_textmessage ON TextMessage(date)

CREATE INDEX date_weather ON Weather(date)

CREATE INDEX date_wifi ON Wifi(date)

CREATE INDEX date_idx_accelerometer ON Accelerometer(participant_id, date)

CREATE INDEX date_idx_airquality ON AirQuality(participant_id, date)

CREATE INDEX date_idx_activity ON Activity(participant_id, date)

CREATE INDEX date_idx_appusage ON AppUsage(participant_id, date)

CREATE INDEX date_idx_battery ON Battery(participant_id, date)

CREATE INDEX date_idx_bluetooth ON Bluetooth(participant_id, date)

CREATE INDEX date_idx_calendar ON Calendar(participant_id, date)

CREATE INDEX date_idx_connectivity ON Connectivity(participant_id, date)

CREATE INDEX date_idx_device ON Device(participant_id, date)

CREATE INDEX date_idx_geofence ON Geofence(participant_id, date)

CREATE INDEX date_idx_gyroscope ON Gyroscope(participant_id, date)

CREATE INDEX date_idx_installedapps ON InstalledApps(participant_id, date)

CREATE INDEX date_idx_keyboard ON Keyboard(participant_id, date)

CREATE INDEX date_idx_light ON Light(participant_id, date)

CREATE INDEX date_idx_location ON Location(participant_id, date)

CREATE INDEX date_idx_memory ON Memory(participant_id, date)

CREATE INDEX date_idx_mobility ON Mobility(participant_id, date)

CREATE INDEX date_idx_noise ON Noise(participant_id, date)

CREATE INDEX date_idx_pedometer ON Pedometer(participant_id, date)

CREATE INDEX date_idx_phonelog ON PhoneLog(participant_id, date)

CREATE INDEX date_idx_screen ON Screen(participant_id, date)

CREATE INDEX date_idx_textmessage ON TextMessage(participant_id, date)

CREATE INDEX date_idx_weather ON Weather(participant_id, date)

CREATE INDEX date_idx_wifi ON Wifi(participant_id, date)
