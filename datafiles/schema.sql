-- PostgreSQL schema for hackage-server data.
--
-- Foreign keys are DEFERRABLE INITIALLY DEFERRED so data can
-- be loaded in any order within a transaction.


------------------------------------------------------------------------
-- Users (users, authtokens, admins)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS users (
  user_id   INT4 PRIMARY KEY,
  user_name TEXT NOT NULL UNIQUE,
  status    TEXT NOT NULL CHECK (status IN ('enabled', 'disabled', 'deleted')),
  auth_info TEXT            -- password hash, null for deleted users
);

CREATE TABLE IF NOT EXISTS authtokens (
  user_id     INT4 NOT NULL,
  token       TEXT NOT NULL,
  description TEXT NOT NULL,
  PRIMARY KEY (user_id, token),
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS admins (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Package hierarchy
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS packages (
  pkg_name TEXT PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS package_versions (
  pkg_name    TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);

-- Tarball uploads (one per version, typically)
CREATE TABLE IF NOT EXISTS package_tarballs (
  pkg_name          TEXT NOT NULL,
  pkg_version       TEXT NOT NULL,
  revision          INT4 NOT NULL,
  upload_time       TIMESTAMPTZ NOT NULL,
  upload_user_id    INT4 NOT NULL,
  tarball_gz_md5    BYTEA NOT NULL,
  tarball_gz_length INT8 NOT NULL,
  tarball_gz_sha256 BYTEA NOT NULL,
  tarball_md5       BYTEA NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, revision),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (upload_user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);

-- Cabal file revisions (uploads.csv)
CREATE TABLE IF NOT EXISTS package_uploads (
  pkg_name       TEXT NOT NULL,
  pkg_version    TEXT NOT NULL,
  revision       INT4 NOT NULL,
  cabal_file     BYTEA NOT NULL,
  upload_time    TIMESTAMPTZ NOT NULL,
  upload_user_id INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, revision),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (upload_user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);

-- Core update log
CREATE TABLE IF NOT EXISTS package_update_log (
  id          BIGSERIAL PRIMARY KEY,
  entry_type  TEXT NOT NULL CHECK (entry_type IN ('cabal', 'metadata', 'extra')),
  pkg_name    TEXT,
  pkg_version TEXT,
  revision    INT4,
  timestamp   TIMESTAMPTZ NOT NULL,
  user_id     INT4,
  user_name   TEXT,
  file_path   TEXT,
  file_data   BYTEA
);


------------------------------------------------------------------------
-- User Details (user-details/users.csv)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS user_details (
  user_id       INT4 PRIMARY KEY,
  real_name     TEXT NOT NULL,
  contact_email TEXT NOT NULL,
  account_kind  TEXT CHECK (account_kind IN ('real', 'special')), -- or null
  admin_notes   TEXT NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Groups: trustees, uploaders, mirrorers (one row per member)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS trustees (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS uploaders (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS mirror_clients (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Package maintainers (maintainers.csv, one row per package/user pair)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS maintainers (
  pkg_name TEXT NOT NULL,
  user_id  INT4 NOT NULL,
  PRIMARY KEY (pkg_name, user_id),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Tags (tags.csv, one row per package/tag pair)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS tags (
  pkg_name TEXT NOT NULL,
  tag      TEXT NOT NULL,
  PRIMARY KEY (pkg_name, tag),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);


CREATE TABLE IF NOT EXISTS tag_reviews (
  pkg_name    TEXT NOT NULL,
  tag         TEXT NOT NULL,
  is_addition BOOL NOT NULL,
  PRIMARY KEY (pkg_name, tag, is_addition),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Tag aliases (tagAliases.csv, one row per canonical/alias pair)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS tag_aliases (
  canonical_tag TEXT NOT NULL,
  alias_tag     TEXT NOT NULL,
  PRIMARY KEY (canonical_tag, alias_tag)
);


------------------------------------------------------------------------
-- Admin log
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS admin_log (
  id                    BIGSERIAL PRIMARY KEY,
  timestamp             TIMESTAMPTZ NOT NULL,
  actor_user_id         INT4 NOT NULL,
  action_type           TEXT NOT NULL CHECK (action_type IN ('add', 'delete')),
  target_user_id        INT4 NOT NULL,
  group_description     TEXT NOT NULL,
  reason                TEXT NOT NULL,
  FOREIGN KEY (actor_user_id)  REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (target_user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Download counts
------------------------------------------------------------------------

-- Today's in-memory download counts
CREATE TABLE IF NOT EXISTS downloads_today (
  pkg_name    TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  count       INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS downloads_today_meta (
  id    INT4 PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  today DATE NOT NULL
);

-- Historical download counts
CREATE TABLE IF NOT EXISTS download_history (
  pkg_name    TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  day         DATE NOT NULL,
  count       INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, day),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Distributions
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS distros (
  name TEXT PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS distro_packages (
  distro_name TEXT NOT NULL,
  pkg_name    TEXT NOT NULL,
  version     TEXT NOT NULL,
  url         TEXT NOT NULL,
  PRIMARY KEY (distro_name, pkg_name),
  FOREIGN KEY (distro_name) REFERENCES distros(name) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS distro_maintainers (
  distro_name TEXT NOT NULL,
  user_id     INT4 NOT NULL,
  PRIMARY KEY (distro_name, user_id),
  FOREIGN KEY (distro_name) REFERENCES distros(name) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Preferred versions
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS deprecated_versions (
  pkg_name TEXT NOT NULL,
  version  TEXT NOT NULL,
  PRIMARY KEY (pkg_name, version),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS deprecated_packages (
  pkg_name    TEXT NOT NULL,
  replacement TEXT NOT NULL,
  PRIMARY KEY (pkg_name, replacement),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS preferred_versions_meta (
  migrated_ephemeral_prefs BOOL NOT NULL
);


------------------------------------------------------------------------
-- User notifications
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS user_notify_prefs (
  user_id                    INT4 PRIMARY KEY,
  opt_out                    BOOL NOT NULL,
  revision_range             TEXT NOT NULL,
  upload                     BOOL NOT NULL,
  maintainer_group           BOOL NOT NULL,
  doc_builder_report         BOOL NOT NULL,
  pending_tags               BOOL NOT NULL,
  dependency_for_maintained  BOOL NOT NULL,
  dependency_trigger_bounds  TEXT NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS user_notify_meta (
  id        INT4 PRIMARY KEY DEFAULT 1,
  last_time TIMESTAMPTZ NOT NULL
);


------------------------------------------------------------------------
-- User signups / password resets
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS signup_entries (
  nonce                TEXT PRIMARY KEY,
  entry_type           TEXT NOT NULL,
  signup_user_name     TEXT,
  signup_real_name     TEXT,
  signup_contact_email TEXT,
  reset_user_id        INT4,
  timestamp            TIMESTAMPTZ NOT NULL,
  FOREIGN KEY (reset_user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Build reports
------------------------------------------------------------------------

CREATE TYPE flag_assignment    AS (flag_name TEXT, flag_value BOOL);
CREATE TYPE package_identifier AS (pkg_name TEXT, pkg_version TEXT);

CREATE TABLE IF NOT EXISTS build_reports (
  pkg_name          TEXT NOT NULL,
  pkg_version       TEXT NOT NULL,
  report_id         INT4 NOT NULL,
  report_time       TIMESTAMPTZ,
  doc_builder       BOOL NOT NULL DEFAULT false,
  os                TEXT NOT NULL,
  arch              TEXT NOT NULL,
  compiler_flavor   TEXT NOT NULL,
  compiler_version  TEXT NOT NULL,
  client_name       TEXT NOT NULL,
  client_version    TEXT NOT NULL,
  flags             flag_assignment[] NOT NULL DEFAULT '{}',
  dependencies      package_identifier[] NOT NULL DEFAULT '{}',
  install_outcome   TEXT NOT NULL CHECK (install_outcome IN (
    'PlanningFailed',
    'DependencyFailed',
    'DownloadFailed',
    'UnpackFailed',
    'SetupFailed',
    'ConfigureFailed',
    'BuildFailed',
    'InstallFailed',
    'InstallOk'
  )),
  dependency_failed_pkg TEXT CHECK (
    (install_outcome = 'DependencyFailed') = (dependency_failed_pkg IS NOT NULL)
  ),
  docs_outcome      TEXT NOT NULL CHECK (docs_outcome IN ('NotTried', 'Failed', 'Ok')),
  tests_outcome     TEXT NOT NULL CHECK (tests_outcome IN ('NotTried', 'Failed', 'Ok')),
  build_log_blob_id BYTEA,
  test_log_blob_id  BYTEA,
  build_covg_text   TEXT,
  test_report_log_blob_id BYTEA,
  PRIMARY KEY (pkg_name, pkg_version, report_id),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS build_report_package_meta (
  pkg_name    TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  fail_count  INT4,
  run_tests   BOOL NOT NULL DEFAULT false,
  PRIMARY KEY (pkg_name, pkg_version),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Security (TUF state)
------------------------------------------------------------------------

-- Singleton table (always exactly one row with id=1).
-- Stores the global TUF security state.
CREATE TABLE IF NOT EXISTS security_state (
  id                INT4 PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  tar_gz_length     INT8 NOT NULL,
  tar_gz_sha256     BYTEA NOT NULL,
  tar_gz_md5        BYTEA,
  tar_length        INT8 NOT NULL,
  tar_sha256        BYTEA NOT NULL,
  tar_md5           BYTEA,
  snapshot_version  INT4 NOT NULL,
  timestamp_version INT4 NOT NULL,
  timestamp_time    TIMESTAMPTZ NOT NULL
);

-- Singleton table (always exactly one row with id=1).
-- Stores TUF keys and cached snapshot/timestamp as a SafeCopy blob.
CREATE TABLE IF NOT EXISTS security_files (
  id         INT4 PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  files_data BYTEA
);


------------------------------------------------------------------------
-- Package candidates
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS candidates (
  pkg_name    TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  is_public   BOOL NOT NULL,
  warnings    TEXT[] NOT NULL DEFAULT '{}',
  migrated_pkg_tarball BOOL NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS candidate_uploads (
  pkg_name       TEXT NOT NULL,
  pkg_version    TEXT NOT NULL,
  revision       INT4 NOT NULL,
  cabal_file     BYTEA NOT NULL,
  upload_time    TIMESTAMPTZ NOT NULL,
  upload_user_id INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, revision),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (upload_user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS candidate_tarballs (
  pkg_name          TEXT NOT NULL,
  pkg_version       TEXT NOT NULL,
  revision          INT4 NOT NULL,
  upload_time       TIMESTAMPTZ NOT NULL,
  upload_user_id    INT4 NOT NULL,
  tarball_gz_md5    BYTEA NOT NULL,
  tarball_gz_length INT8 NOT NULL,
  tarball_gz_sha256 BYTEA NOT NULL,
  tarball_md5       BYTEA NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, revision),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (upload_user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Documentation
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS documentation (
  pkg_name    TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  blob_id     TEXT NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Votes
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS votes (
  pkg_name TEXT NOT NULL,
  user_id  INT4 NOT NULL,
  score    INT4 NOT NULL,
  PRIMARY KEY (pkg_name, user_id),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Analytics pixels
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS analytics_pixels (
  pkg_name  TEXT NOT NULL,
  pixel_url TEXT NOT NULL,
  PRIMARY KEY (pkg_name, pixel_url),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Vouch
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS vouches (
  vouchee_id INT4 NOT NULL,
  voucher_id INT4 NOT NULL,
  vouched_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (vouchee_id, voucher_id),
  FOREIGN KEY (vouchee_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (voucher_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS vouch_not_notified (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Haskell Platform
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS platform_packages (
  pkg_name TEXT NOT NULL,
  version  TEXT NOT NULL,
  PRIMARY KEY (pkg_name, version),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Tar index cache
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS tar_index_cache (
  tarball_blob_id TEXT NOT NULL PRIMARY KEY,
  index_blob_id   TEXT NOT NULL
);


------------------------------------------------------------------------
-- Event tables (event log for each update operation)
--
-- Each update event is logged as a row in its event table.
-- Columns use TEXT via Show serialization of the Haskell types.
-- These tables are the durable log; checkpoint/state tables above
-- are periodic snapshots. When a state type migrates from event-sourcing
-- to direct state storage, its event tables can be dropped.
------------------------------------------------------------------------

-- Users events
CREATE TABLE IF NOT EXISTS users_add_user_enabled (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users_add_user_disabled (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users_set_user_enabled_status (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users_set_user_auth (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users_set_user_name (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users_delete_user (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users_add_auth_token (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users_revoke_auth_token (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_admins_add_hackage_admin (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_admins_remove_hackage_admin (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- Upload events
CREATE TABLE IF NOT EXISTS hackage_trustees_add_hackage_trustee (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_trustees_remove_hackage_trustee (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_uploaders_add_hackage_uploader (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_uploaders_remove_hackage_uploader (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_maintainers_add_package_maintainer (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_maintainers_remove_package_maintainer (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_maintainers_set_package_maintainers (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- Tags events
CREATE TABLE IF NOT EXISTS package_tags_add_package_tag (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags_remove_package_tag (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags_set_package_tags (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags_set_tag_packages (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags_insert_review_tags (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags_clear_review_tags (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS tag_alias_add_tag_alias (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- Documentation events
CREATE TABLE IF NOT EXISTS documentation_insert_documentation (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS documentation_remove_documentation (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- TarIndexCache events
CREATE TABLE IF NOT EXISTS tar_index_cache_set_tar_index (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- AnalyticsPixels events
CREATE TABLE IF NOT EXISTS analytics_pixels_state_add_package_analytics_pixel (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS analytics_pixels_state_remove_package_analytics_pixel (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- UserDetails events
CREATE TABLE IF NOT EXISTS user_details_table_set_user_details (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS user_details_table_set_user_name_contact (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS user_details_table_set_user_admin_info (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS user_details_table_delete_user_details (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- UserNotify events
CREATE TABLE IF NOT EXISTS notify_data_add_notify_pref (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS notify_data_set_notify_time (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- UserSignup events
CREATE TABLE IF NOT EXISTS signup_reset_table_add_signup_reset_info (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS signup_reset_table_delete_signup_reset_info (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS signup_reset_table_delete_all_expired (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- AdminLog events
CREATE TABLE IF NOT EXISTS admin_log_add_admin_log (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- Vouch events
CREATE TABLE IF NOT EXISTS vouch_data_put_vouch (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- PreferredVersions events
CREATE TABLE IF NOT EXISTS preferred_versions_set_preferred_ranges (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS preferred_versions_set_deprecated_versions (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS preferred_versions_set_deprecated_for (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- Distro events
CREATE TABLE IF NOT EXISTS distros_add_distro (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros_remove_distro (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros_add_distro_maintainer (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros_remove_distro_maintainer (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros_put_distro_package_list (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros_drop_package (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- DownloadCount events
CREATE TABLE IF NOT EXISTS in_mem_stats_register_download (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- BuildReports events
CREATE TABLE IF NOT EXISTS build_reports_add_report (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports_set_build_log (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports_delete_report (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports_set_fail_status (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports_reset_fail_count (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports_set_test_log (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports_set_run_tests (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- PackageCandidates events
CREATE TABLE IF NOT EXISTS candidate_packages_add_candidate (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS candidate_packages_delete_candidate (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS candidate_packages_delete_candidates (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
