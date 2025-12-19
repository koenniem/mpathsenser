CREATE INDEX IF NOT EXISTS idx_accelerometer ON Accelerometer(participant_id);

CREATE INDEX IF NOT EXISTS idx_airquality ON AirQuality(participant_id);

CREATE INDEX IF NOT EXISTS idx_activity ON Activity(participant_id);

CREATE INDEX IF NOT EXISTS idx_appusage ON AppUsage(participant_id);

CREATE INDEX IF NOT EXISTS idx_battery ON Battery(participant_id);

CREATE INDEX IF NOT EXISTS idx_bluetooth ON Bluetooth(participant_id);

CREATE INDEX IF NOT EXISTS idx_calendar ON Calendar(participant_id);

CREATE INDEX IF NOT EXISTS idx_connectivity ON Connectivity(participant_id);

CREATE INDEX IF NOT EXISTS idx_device ON Device(participant_id);

CREATE INDEX IF NOT EXISTS idx_error ON Error(participant_id);

CREATE INDEX IF NOT EXISTS idx_garminbbi ON GarminBBI(participant_id);

CREATE INDEX IF NOT EXISTS idx_garminheartrate ON GarminHeartRate(participant_id);

CREATE INDEX IF NOT EXISTS idx_garminmeta ON GarminMeta(participant_id);

CREATE INDEX IF NOT EXISTS idx_garminsteps ON GarminSteps(participant_id);

CREATE INDEX IF NOT EXISTS idx_garminstress ON GarminStress(participant_id);

CREATE INDEX IF NOT EXISTS idx_geofence ON Geofence(participant_id);

CREATE INDEX IF NOT EXISTS idx_gyroscope ON Gyroscope(participant_id);

CREATE INDEX IF NOT EXISTS idx_heartbeat ON Heartbeat(participant_id);

CREATE INDEX IF NOT EXISTS idx_installedapps ON InstalledApps(participant_id);

CREATE INDEX IF NOT EXISTS idx_keyboard ON Keyboard(participant_id);

CREATE INDEX IF NOT EXISTS idx_light ON Light(participant_id);

CREATE INDEX IF NOT EXISTS idx_location ON Location(participant_id);

CREATE INDEX IF NOT EXISTS idx_memory ON Memory(participant_id);

CREATE INDEX IF NOT EXISTS idx_mobility ON Mobility(participant_id);

CREATE INDEX IF NOT EXISTS idx_noise ON Noise(participant_id);

CREATE INDEX IF NOT EXISTS idx_pedometer ON Pedometer(participant_id);

CREATE INDEX IF NOT EXISTS idx_phonelog ON PhoneLog(participant_id);

CREATE INDEX IF NOT EXISTS idx_screen ON Screen(participant_id);

CREATE INDEX IF NOT EXISTS idx_textmessage ON TextMessage(participant_id);

CREATE INDEX IF NOT EXISTS idx_timezone ON Timezone(participant_id);

CREATE INDEX IF NOT EXISTS idx_weather ON Weather(participant_id);

CREATE INDEX IF NOT EXISTS idx_wifi ON Wifi(participant_id);

CREATE INDEX IF NOT EXISTS date_accelerometer ON Accelerometer(date);

CREATE INDEX IF NOT EXISTS date_airquality ON AirQuality(date);

CREATE INDEX IF NOT EXISTS date_activity ON Activity(date);

CREATE INDEX IF NOT EXISTS date_appusage ON AppUsage(date);

CREATE INDEX IF NOT EXISTS date_battery ON Battery(date);

CREATE INDEX IF NOT EXISTS date_bluetooth ON Bluetooth(date);

CREATE INDEX IF NOT EXISTS date_calendar ON Calendar(date);

CREATE INDEX IF NOT EXISTS date_connectivity ON Connectivity(date);

CREATE INDEX IF NOT EXISTS date_device ON Device(date);

CREATE INDEX IF NOT EXISTS date_error ON Error(date);

CREATE INDEX IF NOT EXISTS date_garminbbi ON GarminBBI(date);

CREATE INDEX IF NOT EXISTS date_garminheartrate ON GarminHeartRate(date);

CREATE INDEX IF NOT EXISTS date_garminmeta ON GarminMeta(date);

CREATE INDEX IF NOT EXISTS date_garminsteps ON GarminSteps(date);

CREATE INDEX IF NOT EXISTS date_garminstress ON GarminStress(date);

CREATE INDEX IF NOT EXISTS date_geofence ON Geofence(date);

CREATE INDEX IF NOT EXISTS date_gyroscope ON Gyroscope(date);

CREATE INDEX IF NOT EXISTS date_heartbeat ON Heartbeat(date);

CREATE INDEX IF NOT EXISTS date_installedapps ON InstalledApps(date);

CREATE INDEX IF NOT EXISTS date_keyboard ON Keyboard(date);

CREATE INDEX IF NOT EXISTS date_light ON Light(date);

CREATE INDEX IF NOT EXISTS date_location ON Location(date);

CREATE INDEX IF NOT EXISTS date_memory ON Memory(date);

CREATE INDEX IF NOT EXISTS date_mobility ON Mobility(date);

CREATE INDEX IF NOT EXISTS date_noise ON Noise(date);

CREATE INDEX IF NOT EXISTS date_pedometer ON Pedometer(date);

CREATE INDEX IF NOT EXISTS date_phonelog ON PhoneLog(date);

CREATE INDEX IF NOT EXISTS date_screen ON Screen(date);

CREATE INDEX IF NOT EXISTS date_textmessage ON TextMessage(date);

CREATE INDEX IF NOT EXISTS date_timezone ON Timezone(date);

CREATE INDEX IF NOT EXISTS date_weather ON Weather(date);

CREATE INDEX IF NOT EXISTS date_wifi ON Wifi(date);

CREATE INDEX IF NOT EXISTS date_idx_accelerometer ON Accelerometer(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_airquality ON AirQuality(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_activity ON Activity(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_appusage ON AppUsage(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_battery ON Battery(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_bluetooth ON Bluetooth(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_calendar ON Calendar(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_connectivity ON Connectivity(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_device ON Device(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_error ON Error(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_garminbbi ON GarminBBI(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_garminheartrate ON GarminHeartRate(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_garminmeta ON GarminMeta(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_garminsteps ON GarminSteps(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_garminstress ON GarminStress(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_geofence ON Geofence(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_gyroscope ON Gyroscope(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_heartbeat ON Heartbeat(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_installedapps ON InstalledApps(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_keyboard ON Keyboard(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_light ON Light(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_location ON Location(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_memory ON Memory(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_mobility ON Mobility(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_noise ON Noise(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_pedometer ON Pedometer(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_phonelog ON PhoneLog(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_screen ON Screen(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_textmessage ON TextMessage(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_timezone ON Timezone(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_weather ON Weather(participant_id, date);

CREATE INDEX IF NOT EXISTS date_idx_wifi ON Wifi(participant_id, date);
