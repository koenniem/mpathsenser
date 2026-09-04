-- Sequence backing ProcessedFiles.file_id. Never reset this sequence in normal
-- operation: file_id ordering is used to decide which file wins on deduplication.
CREATE SEQUENCE IF NOT EXISTS processed_files_seq START 1;

CREATE TABLE IF NOT EXISTS Study(
  study_id TEXT NOT NULL,
  data_format TEXT,
  PRIMARY KEY(study_id)
);

CREATE TABLE IF NOT EXISTS Participant(
  participant_id UINTEGER NOT NULL,
  study_id TEXT NOT NULL,
  PRIMARY KEY(participant_id),
  FOREIGN KEY(study_id) REFERENCES Study(study_id)
);

CREATE TABLE IF NOT EXISTS ProcessedFiles(
  file_id UBIGINT NOT NULL DEFAULT nextval('processed_files_seq'),
  file_name TEXT NOT NULL,
  participant_id UINTEGER NOT NULL,
  sense_version UTINYINT,
  file_size_bytes UBIGINT,
  modified_at TIMESTAMPTZ,
  processed_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
  PRIMARY KEY(file_id),
  UNIQUE(file_name, participant_id, file_size_bytes, modified_at),
  FOREIGN KEY(participant_id) REFERENCES Participant(participant_id)
);

CREATE TABLE IF NOT EXISTS Accelerometer(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ,
  n USMALLINT,
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
  x_neg_n USMALLINT,
  y_neg_n USMALLINT,
  z_neg_n USMALLINT,
  x_pos_n USMALLINT,
  y_pos_n USMALLINT,
  z_pos_n USMALLINT,
  x_above_mean USMALLINT,
  y_above_mean USMALLINT,
  z_above_mean USMALLINT,
  x_energy REAL,
  y_energy REAL,
  z_energy REAL,
  avg_res_acc REAL,
  sma REAL,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Activity(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  confidence UTINYINT,
  type TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS AppUsage(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ,
  period_start TIMESTAMPTZ,
  period_end TIMESTAMPTZ,
  usage UBIGINT,
  app TEXT,
  package_name TEXT,
  last_foreground TIMESTAMPTZ,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Battery(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  battery_level UTINYINT,
  battery_status TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Bluetooth(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  start_scan TIMESTAMPTZ,
  end_scan TIMESTAMPTZ,
  advertisement_name TEXT,
  bluetooth_device_id TEXT,
  bluetooth_device_name TEXT,
  connectable BOOLEAN,
  rssi SMALLINT,
  tx_power_level SMALLINT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS BluetoothBeacon(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  region TEXT,
  uuid TEXT,
  rssi SMALLINT,
  major USMALLINT,
  minor USMALLINT,
  accuracy REAL,
  proximity TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Connectivity(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  connectivity_status TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Device(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  device_id TEXT,
  hardware TEXT,
  device_name TEXT,
  device_manufacturer TEXT,
  device_model TEXT,
  operating_system TEXT,
  platform TEXT,
  operating_system_version TEXT,
  device_data JSON,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Error(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  message TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminAccelerometer(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  x REAL,
  y REAL,
  z REAL,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminActigraphy(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ,
  instance TEXT,
  total_energy INTEGER,
  n_zero_crossing INTEGER,
  time_above_threshold REAL,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminBBI(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  bbi USMALLINT,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminEnhancedBBI(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  bbi USMALLINT,
  status TEXT,
  gap_duration INTEGER,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminGyroscope(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  x REAL,
  y REAL,
  z REAL,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminHeartRate(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  bpm USMALLINT,
  status TEXT,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminMeta(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  time_from TIMESTAMPTZ,
  time_to TIMESTAMPTZ,
  n_accelerometer UINTEGER,
  n_actigraphy_1 UINTEGER,
  n_actigraphy_2 UINTEGER,
  n_actigraphy_3 UINTEGER,
  n_bbi UINTEGER,
  n_enhanced_bbi UINTEGER,
  n_gyroscope UINTEGER,
  n_heartrate UINTEGER,
  n_respiration UINTEGER,
  n_skin_temperature UINTEGER,
  n_spo2 UINTEGER,
  n_steps UINTEGER,
  n_stress UINTEGER,
  n_wrist_status UINTEGER,
  n_zero_crossing UINTEGER,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminRespiration(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  bpm REAL,
  status TEXT,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminSkinTemperature(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  temperature REAL,
  status TEXT,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminSPO2(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  spo2 UTINYINT,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminSteps(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ,
  step_count USMALLINT,
  total_steps UINTEGER,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminStress(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  stress UTINYINT,
  status TEXT,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminWristStatus(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  status TEXT,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS GarminZeroCrossing(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ,
  total_energy UINTEGER,
  n_zero_crossing UINTEGER,
  deadband INTEGER,
  mac_address TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Heartbeat(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  period INTEGER,
  device_type TEXT,
  device_role_name TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Light(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  end_time TIMESTAMPTZ,
  mean_lux REAL,
  std_lux REAL,
  min_lux REAL,
  max_lux REAL,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Location(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  latitude DOUBLE,
  longitude DOUBLE,
  altitude REAL,
  accuracy REAL,
  vertical_accuracy REAL,
  speed REAL,
  speed_accuracy REAL,
  heading REAL,
  heading_accuracy REAL,
  is_mock BOOLEAN,
  elapsed_realtime_nanos UBIGINT,
  elapsed_realtime_uncertainty_nanos UBIGINT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Memory(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  free_physical_memory UBIGINT,
  free_virtual_memory UBIGINT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Pedometer(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  step_count UINTEGER,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Screen(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  screen_event TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Timezone(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Weather(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  country TEXT,
  area_name TEXT,
  weather_main TEXT,
  weather_description TEXT,
  sunrise TIMESTAMPTZ,
  sunset TIMESTAMPTZ,
  latitude DOUBLE,
  longitude DOUBLE,
  pressure REAL,
  wind_speed REAL,
  wind_degree REAL,
  humidity REAL,
  cloudiness REAL,
  rain_last_hour REAL,
  rain_last_3hours REAL,
  snow_last_hour REAL,
  snow_last_3hours REAL,
  temperature REAL,
  temp_min REAL,
  temp_max REAL,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Wifi(
  participant_id UINTEGER NOT NULL,
  time TIMESTAMPTZ NOT NULL,
  ssid TEXT,
  bssid TEXT,
  ip TEXT,
  timezone TEXT,
  source_file_id UBIGINT NOT NULL
);



CREATE TABLE IF NOT EXISTS Meta(
  key TEXT PRIMARY KEY,
  value TEXT
);
