-- Hackage Server PostgreSQL Schema
--
-- This file contains all CREATE TABLE statements for the hackage-server
-- database. It is executed on startup to ensure all tables exist.
--
-- Tables are grouped by feature and use CREATE TABLE IF NOT EXISTS for
-- idempotent execution. Foreign key constraints are noted as TODO
-- comments where they should exist but are not yet enforced.


------------------------------------------------------------------------
-- Feature: Users
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS users__accounts (
  user_id INT4 PRIMARY KEY,
  user_name TEXT NOT NULL,
  user_status TEXT NOT NULL,
  auth_hash TEXT
);

CREATE TABLE IF NOT EXISTS users__tokens (
  user_id INT4 NOT NULL,
  token TEXT NOT NULL,
  description TEXT NOT NULL,
  PRIMARY KEY (user_id, token),
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS users__admins (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: Core (packages)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS packages__cabal_revisions (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  revision INT4 NOT NULL,
  cabal_file_data BYTEA NOT NULL,
  upload_time TIMESTAMPTZ NOT NULL,
  upload_user_id INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, revision),
  FOREIGN KEY (upload_user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS packages__tarballs (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  revision INT4 NOT NULL,
  tarball_gz_blob_id TEXT NOT NULL,
  tarball_gz_length INT8 NOT NULL,
  tarball_gz_sha256 TEXT NOT NULL,
  tarball_nogz_blob_id TEXT NOT NULL,
  upload_time TIMESTAMPTZ NOT NULL,
  upload_user_id INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, revision),
  FOREIGN KEY (upload_user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS packages__update_log (
  id BIGSERIAL PRIMARY KEY,
  entry_type TEXT NOT NULL,
  pkg_name TEXT,
  pkg_version TEXT,
  revision INT4,
  timestamp TIMESTAMPTZ NOT NULL,
  user_id INT4,
  user_name TEXT,
  file_path TEXT,
  file_data BYTEA
);


------------------------------------------------------------------------
-- Feature: PackageCandidates
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS candidates__cabal_revisions (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  revision INT4 NOT NULL,
  cabal_file_data BYTEA NOT NULL,
  upload_time TIMESTAMPTZ NOT NULL,
  upload_user_id INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, revision),
  FOREIGN KEY (upload_user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS candidates__tarballs (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  revision INT4 NOT NULL,
  tarball_gz_blob_id TEXT NOT NULL,
  tarball_gz_length INT8 NOT NULL,
  tarball_gz_sha256 TEXT NOT NULL,
  tarball_nogz_blob_id TEXT NOT NULL,
  upload_time TIMESTAMPTZ NOT NULL,
  upload_user_id INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, revision),
  FOREIGN KEY (upload_user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS candidates__meta (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  warnings TEXT NOT NULL,
  is_public BOOL NOT NULL,
  migrated_pkg_tarball BOOL NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version)
);


------------------------------------------------------------------------
-- Feature: Mirror
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS mirror__clients (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: Upload
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS upload__trustees (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS upload__uploaders (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS upload__maintainers (
  pkg_name TEXT NOT NULL,
  user_id INT4 NOT NULL,
  PRIMARY KEY (pkg_name, user_id),
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: Tags
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS tags__assignments (
  pkg_name TEXT NOT NULL,
  tag TEXT NOT NULL,
  PRIMARY KEY (pkg_name, tag)
);

CREATE TABLE IF NOT EXISTS tags__reviews (
  pkg_name TEXT NOT NULL,
  tag TEXT NOT NULL,
  is_addition BOOL NOT NULL,
  PRIMARY KEY (pkg_name, tag, is_addition)
);

CREATE TABLE IF NOT EXISTS tags__aliases (
  canonical_tag TEXT NOT NULL,
  alias_tag TEXT NOT NULL,
  PRIMARY KEY (canonical_tag, alias_tag)
);


------------------------------------------------------------------------
-- Feature: Documentation
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS documentation__docs (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  blob_id TEXT NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version)
);


------------------------------------------------------------------------
-- Feature: Votes
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS votes__votes (
  pkg_name TEXT NOT NULL,
  user_id INT4 NOT NULL,
  score INT4 NOT NULL,
  PRIMARY KEY (pkg_name, user_id),
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: HaskellPlatform
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS platform__packages (
  pkg_name TEXT NOT NULL,
  version TEXT NOT NULL,
  PRIMARY KEY (pkg_name, version)
);


------------------------------------------------------------------------
-- Feature: TarIndexCache
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS tar_index_cache__cache (
  tarball_blob_id TEXT NOT NULL PRIMARY KEY,
  index_blob_id TEXT NOT NULL
);


------------------------------------------------------------------------
-- Feature: AnalyticsPixels
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS analytics__pixels (
  pkg_name TEXT NOT NULL,
  pixel_url TEXT NOT NULL,
  PRIMARY KEY (pkg_name, pixel_url)
);


------------------------------------------------------------------------
-- Feature: UserDetails
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS user_details__details (
  user_id INT4 PRIMARY KEY,
  name TEXT NOT NULL,
  contact_email TEXT NOT NULL,
  account_kind TEXT,
  admin_notes TEXT NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: UserNotify
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS user_notify__prefs (
  user_id INT4 PRIMARY KEY,
  opt_out BOOL NOT NULL,
  revision_range TEXT NOT NULL,
  upload BOOL NOT NULL,
  maintainer_group BOOL NOT NULL,
  doc_builder_report BOOL NOT NULL,
  pending_tags BOOL NOT NULL,
  dependency_for_maintained BOOL NOT NULL,
  dependency_trigger_bounds TEXT NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS user_notify__meta (
  id INT4 PRIMARY KEY DEFAULT 1,
  last_time TIMESTAMPTZ NOT NULL
);


------------------------------------------------------------------------
-- Feature: UserSignup
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS user_signup__entries (
  nonce TEXT PRIMARY KEY,
  entry_type TEXT NOT NULL,
  signup_user_name TEXT,
  signup_real_name TEXT,
  signup_contact_email TEXT,
  reset_user_id INT4,
  timestamp TIMESTAMPTZ NOT NULL,
  FOREIGN KEY (reset_user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: AdminLog
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS admin_log__entries (
  id BIGSERIAL PRIMARY KEY,
  timestamp TIMESTAMPTZ NOT NULL,
  user_id INT4 NOT NULL,
  action_type TEXT NOT NULL,
  action_target_user_id INT4,
  group_type TEXT NOT NULL,
  group_data TEXT,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (action_target_user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: Vouch
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS vouch__vouches (
  vouchee_id INT4 NOT NULL,
  voucher_id INT4 NOT NULL,
  vouched_at TIMESTAMPTZ NOT NULL,
  PRIMARY KEY (vouchee_id, voucher_id),
  FOREIGN KEY (vouchee_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (voucher_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS vouch__not_notified (
  user_id INT4 PRIMARY KEY,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: PreferredVersions
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS preferred_versions__deprecated_versions (
  pkg_name TEXT NOT NULL,
  version TEXT NOT NULL,
  PRIMARY KEY (pkg_name, version)
);

CREATE TABLE IF NOT EXISTS preferred_versions__deprecated_packages (
  pkg_name TEXT NOT NULL,
  replacement TEXT NOT NULL,
  PRIMARY KEY (pkg_name, replacement)
);

CREATE TABLE IF NOT EXISTS preferred_versions__meta (
  migrated_ephemeral_prefs BOOL NOT NULL
);


------------------------------------------------------------------------
-- Feature: Distro
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS distro__distros (
  name TEXT PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS distro__maintainers (
  distro_name TEXT NOT NULL,
  user_id INT4 NOT NULL,
  PRIMARY KEY (distro_name, user_id),
  FOREIGN KEY (distro_name) REFERENCES distro__distros(name) DEFERRABLE INITIALLY DEFERRED,
  FOREIGN KEY (user_id) REFERENCES users__accounts(user_id) DEFERRABLE INITIALLY DEFERRED
);

CREATE TABLE IF NOT EXISTS distro__versions (
  distro_name TEXT NOT NULL,
  pkg_name TEXT NOT NULL,
  version TEXT NOT NULL,
  url TEXT NOT NULL,
  PRIMARY KEY (distro_name, pkg_name),
  FOREIGN KEY (distro_name) REFERENCES distro__distros(name) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Feature: DownloadCount
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS download_count__inmem (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  count INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version)
);

CREATE TABLE IF NOT EXISTS download_count__meta (
  today DATE NOT NULL
);


------------------------------------------------------------------------
-- Feature: Security
------------------------------------------------------------------------

-- Singleton table (always exactly one row with id=1).
-- Stores the global TUF security state.
CREATE TABLE IF NOT EXISTS security__state (
  id INT4 PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  tar_gz_length INT8 NOT NULL DEFAULT 0,
  tar_gz_sha256 TEXT NOT NULL DEFAULT '',
  tar_gz_md5 TEXT,
  tar_length INT8 NOT NULL DEFAULT 0,
  tar_sha256 TEXT NOT NULL DEFAULT '',
  tar_md5 TEXT,
  snapshot_version INT4 NOT NULL DEFAULT 0,
  timestamp_version INT4 NOT NULL DEFAULT 0,
  timestamp_time TIMESTAMPTZ NOT NULL DEFAULT '1970-01-01 00:00:00+00'
);

-- Singleton table (always exactly one row with id=1).
-- Stores TUF keys and cached snapshot/timestamp as a SafeCopy blob.
CREATE TABLE IF NOT EXISTS security__files (
  id INT4 PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  files_data BYTEA
);


------------------------------------------------------------------------
-- Feature: BuildReports
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS build_reports__reports (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  report_id INT4 NOT NULL,
  report_text TEXT NOT NULL,
  build_log_blob_id TEXT,
  test_log_blob_id TEXT,
  build_covg_text TEXT,
  PRIMARY KEY (pkg_name, pkg_version, report_id)
);

CREATE TABLE IF NOT EXISTS build_reports__package_meta (
  pkg_name TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  fail_count INT4,
  run_tests BOOL NOT NULL DEFAULT false,
  PRIMARY KEY (pkg_name, pkg_version)
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
CREATE TABLE IF NOT EXISTS users__add_user_enabled (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users__add_user_disabled (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users__set_user_enabled_status (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users__set_user_auth (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users__set_user_name (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users__delete_user (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users__add_auth_token (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS users__revoke_auth_token (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_admins__add_hackage_admin (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_admins__remove_hackage_admin (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- Upload events
CREATE TABLE IF NOT EXISTS hackage_trustees__add_hackage_trustee (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_trustees__remove_hackage_trustee (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_uploaders__add_hackage_uploader (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS hackage_uploaders__remove_hackage_uploader (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_maintainers__add_package_maintainer (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_maintainers__remove_package_maintainer (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_maintainers__set_package_maintainers (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- Tags events
CREATE TABLE IF NOT EXISTS package_tags__add_package_tag (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags__remove_package_tag (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags__set_package_tags (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags__set_tag_packages (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags__insert_review_tags (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS package_tags__clear_review_tags (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS tag_alias__add_tag_alias (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- Documentation events
CREATE TABLE IF NOT EXISTS documentation__insert_documentation (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS documentation__remove_documentation (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- TarIndexCache events
CREATE TABLE IF NOT EXISTS tar_index_cache__set_tar_index (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);


-- UserDetails events
CREATE TABLE IF NOT EXISTS user_details_table__set_user_details (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS user_details_table__set_user_name_contact (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS user_details_table__set_user_admin_info (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS user_details_table__delete_user_details (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- UserNotify events
CREATE TABLE IF NOT EXISTS notify_data__add_notify_pref (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS notify_data__set_notify_time (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- UserSignup events
CREATE TABLE IF NOT EXISTS signup_reset_table__add_signup_reset_info (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS signup_reset_table__delete_signup_reset_info (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS signup_reset_table__delete_all_expired (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);


-- PreferredVersions events
CREATE TABLE IF NOT EXISTS preferred_versions__set_preferred_ranges (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS preferred_versions__set_deprecated_versions (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS preferred_versions__set_deprecated_for (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- Distro events
CREATE TABLE IF NOT EXISTS distros__add_distro (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros__remove_distro (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros__add_distro_maintainer (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros__remove_distro_maintainer (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros__put_distro_package_list (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS distros__drop_package (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- DownloadCount events
CREATE TABLE IF NOT EXISTS in_mem_stats__register_download (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);

-- BuildReports events
CREATE TABLE IF NOT EXISTS build_reports__add_report (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports__set_build_log (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports__delete_report (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports__set_fail_status (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports__reset_fail_count (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports__set_test_log (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL, arg2 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS build_reports__set_run_tests (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL, arg1 TEXT NOT NULL);

-- PackageCandidates events
CREATE TABLE IF NOT EXISTS candidate_packages__add_candidate (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS candidate_packages__delete_candidate (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
CREATE TABLE IF NOT EXISTS candidate_packages__delete_candidates (id BIGSERIAL PRIMARY KEY, arg0 TEXT NOT NULL);
