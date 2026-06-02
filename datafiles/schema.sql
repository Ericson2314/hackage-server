-- PostgreSQL schema for hackage-server data.
--
-- Most tables corresponds to a CSV file produced by the backup system.
--
-- CSV backup files can be loaded into these tables via COPY.
-- Foreign keys are DEFERRABLE INITIALLY DEFERRED so data can
-- be loaded in any order within a transaction.


------------------------------------------------------------------------
-- Users (users.csv, authtokens.csv)
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

-- Tarball uploads (tarball.csv per package, typically one per version)
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

-- Tarball uploads (uploads.csv)
--
-- TODO clarify the relation with `package_tarballs`.
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

-- Core update log (updatelog.csv)
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
-- Admin log (adminLog.txt, parsed into columns)
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
-- Download counts (ondisk.csv, inmem.csv)
------------------------------------------------------------------------

-- Today's in-memory download counts (inmem.csv)
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

-- Historical download counts (ondisk.csv)
CREATE TABLE IF NOT EXISTS download_history (
  pkg_name    TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  day         DATE NOT NULL,
  count       INT4 NOT NULL,
  PRIMARY KEY (pkg_name, pkg_version, day),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Distributions (packages/*.csv, maintainers.csv)
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
-- Preferred versions (package/*/preferred.csv, package/*/deprecated.csv)
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


------------------------------------------------------------------------
-- User notifications (notifydata.csv)
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


------------------------------------------------------------------------
-- User signups (signups.csv, resets.csv)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS signup_requests (
  nonce      TEXT PRIMARY KEY,
  user_name  TEXT NOT NULL,
  real_name  TEXT NOT NULL,
  email      TEXT NOT NULL,
  timestamp  TIMESTAMPTZ NOT NULL
);

CREATE TABLE IF NOT EXISTS password_resets (
  nonce     TEXT PRIMARY KEY,
  user_id   INT4 NOT NULL,
  timestamp TIMESTAMPTZ NOT NULL,
  FOREIGN KEY (user_id) REFERENCES users(user_id) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Build reports (package/*/N.txt, parsed into columns)
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
-- Security (partialstate.csv, singleton table)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS security_state (
  id                INT4 PRIMARY KEY DEFAULT 1 CHECK (id = 1),
  timestamp_version INT4 NOT NULL,
  snapshot_version  INT4 NOT NULL,
  update_time       TIMESTAMPTZ NOT NULL,
  tar_gz_length     INT8 NOT NULL,
  tar_gz_sha256     BYTEA NOT NULL,
  tar_gz_md5        BYTEA,
  tar_length        INT8 NOT NULL,
  tar_sha256        BYTEA NOT NULL,
  tar_md5           BYTEA
);


------------------------------------------------------------------------
-- Package candidates (package/*/candidate.csv)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS candidates (
  pkg_name    TEXT NOT NULL,
  pkg_version TEXT NOT NULL,
  is_public   BOOL NOT NULL,
  warnings    TEXT[] NOT NULL DEFAULT '{}',
  PRIMARY KEY (pkg_name, pkg_version),
  FOREIGN KEY (pkg_name, pkg_version) REFERENCES package_versions(pkg_name, pkg_version) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Votes (votes.csv)
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
-- Analytics pixels (analyticsPixels.csv)
------------------------------------------------------------------------

CREATE TABLE IF NOT EXISTS analytics_pixels (
  pkg_name  TEXT NOT NULL,
  pixel_url TEXT NOT NULL,
  PRIMARY KEY (pkg_name, pixel_url),
  FOREIGN KEY (pkg_name) REFERENCES packages(pkg_name) DEFERRABLE INITIALLY DEFERRED
);


------------------------------------------------------------------------
-- Vouch (vouches.csv, vouchNotNotified.csv)
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
