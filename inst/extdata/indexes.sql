CREATE INDEX idx_accelerometer ON Accelerometer(participant_id);

CREATE INDEX idx_airquality ON AirQuality(participant_id);

CREATE INDEX idx_activity ON Activity(participant_id);

CREATE INDEX idx_appusage ON AppUsage(participant_id);

CREATE INDEX idx_battery ON Battery(participant_id);

CREATE INDEX idx_bluetooth ON Bluetooth(participant_id);

CREATE INDEX idx_calendar ON Calendar(participant_id);

CREATE INDEX idx_connectivity ON Connectivity(participant_id);

CREATE INDEX idx_device ON Device(participant_id);

CREATE INDEX idx_error ON Error(participant_id);

CREATE INDEX idx_geofence ON Geofence(participant_id);

CREATE INDEX idx_gyroscope ON Gyroscope(participant_id);

CREATE INDEX idx_heartbeat ON Heartbeat(participant_id);

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

CREATE INDEX idx_timezone ON Timezone(participant_id);

CREATE INDEX idx_weather ON Weather(participant_id);

CREATE INDEX idx_wifi ON Wifi(participant_id);

CREATE INDEX time_accelerometer ON Accelerometer(time);

CREATE INDEX time_airquality ON AirQuality(time);

CREATE INDEX time_activity ON Activity(time);

CREATE INDEX time_appusage ON AppUsage(time);

CREATE INDEX time_battery ON Battery(time);

CREATE INDEX time_bluetooth ON Bluetooth(time);

CREATE INDEX time_calendar ON Calendar(time);

CREATE INDEX time_connectivity ON Connectivity(time);

CREATE INDEX time_device ON Device(time);

CREATE INDEX time_error ON Error(time);

CREATE INDEX time_geofence ON Geofence(time);

CREATE INDEX time_gyroscope ON Gyroscope(time);

CREATE INDEX time_heartbeat ON Heartbeat(time);

CREATE INDEX time_installedapps ON InstalledApps(time);

CREATE INDEX time_keyboard ON Keyboard(time);

CREATE INDEX time_light ON Light(time);

CREATE INDEX time_location ON Location(time);

CREATE INDEX time_memory ON Memory(time);

CREATE INDEX time_mobility ON Mobility(time);

CREATE INDEX time_noise ON Noise(time);

CREATE INDEX time_pedometer ON Pedometer(time);

CREATE INDEX time_phonelog ON PhoneLog(time);

CREATE INDEX time_screen ON Screen(time);

CREATE INDEX time_textmessage ON TextMessage(time);

CREATE INDEX time_timezone ON Timezone(time);

CREATE INDEX time_weather ON Weather(time);

CREATE INDEX time_wifi ON Wifi(time);

CREATE INDEX time_idx_accelerometer ON Accelerometer(participant_id, time);

CREATE INDEX time_idx_airquality ON AirQuality(participant_id, time);

CREATE INDEX time_idx_activity ON Activity(participant_id, time);

CREATE INDEX time_idx_appusage ON AppUsage(participant_id, time);

CREATE INDEX time_idx_battery ON Battery(participant_id, time);

CREATE INDEX time_idx_bluetooth ON Bluetooth(participant_id, time);

CREATE INDEX time_idx_calendar ON Calendar(participant_id, time);

CREATE INDEX time_idx_connectivity ON Connectivity(participant_id, time);

CREATE INDEX time_idx_device ON Device(participant_id, time);

CREATE INDEX time_idx_error ON Error(participant_id, time);

CREATE INDEX time_idx_geofence ON Geofence(participant_id, time);

CREATE INDEX time_idx_gyroscope ON Gyroscope(participant_id, time);

CREATE INDEX time_idx_heartbeat ON Heartbeat(participant_id, time);

CREATE INDEX time_idx_installedapps ON InstalledApps(participant_id, time);

CREATE INDEX time_idx_keyboard ON Keyboard(participant_id, time);

CREATE INDEX time_idx_light ON Light(participant_id, time);

CREATE INDEX time_idx_location ON Location(participant_id, time);

CREATE INDEX time_idx_memory ON Memory(participant_id, time);

CREATE INDEX time_idx_mobility ON Mobility(participant_id, time);

CREATE INDEX time_idx_noise ON Noise(participant_id, time);

CREATE INDEX time_idx_pedometer ON Pedometer(participant_id, time);

CREATE INDEX time_idx_phonelog ON PhoneLog(participant_id, time);

CREATE INDEX time_idx_screen ON Screen(participant_id, time);

CREATE INDEX time_idx_textmessage ON TextMessage(participant_id, time);

CREATE INDEX time_idx_timezone ON Timezone(participant_id, time);

CREATE INDEX time_idx_weather ON Weather(participant_id, time);

CREATE INDEX time_idx_wifi ON Wifi(participant_id, time);