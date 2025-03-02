CREATE DATABASE IF NOT EXISTS db_data
  DEFAULT
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

CREATE DATABASE IF NOT EXISTS db_log
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

USE db_log;

CREATE TABLE IF NOT EXISTS `appLog` (
  `id` int(9) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(127) COLLATE utf8_danish_ci DEFAULT NULL,
  `name` varchar(255) COLLATE utf8_danish_ci DEFAULT NULL,
  `group` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `role` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `resh_id` varchar(31) COLLATE utf8_danish_ci DEFAULT NULL,
  `message` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;


CREATE TABLE IF NOT EXISTS `reportLog` (
  `id` int(9) unsigned NOT NULL AUTO_INCREMENT,
  `time` datetime DEFAULT NULL,
  `user` varchar(127) COLLATE utf8_danish_ci DEFAULT NULL,
  `name` varchar(255) COLLATE utf8_danish_ci DEFAULT NULL,
  `group` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `role` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `resh_id` varchar(31) COLLATE utf8_danish_ci DEFAULT NULL,
  `environment` varchar(63) COLLATE utf8_danish_ci DEFAULT NULL,
  `call` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  `message` varchar(2047) COLLATE utf8_danish_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_danish_ci;


CREATE DATABASE IF NOT EXISTS db_autoreport
  CHARACTER SET = 'utf8'
  COLLATE = 'utf8_danish_ci';

use db_autoreport;

CREATE TABLE IF NOT EXISTS `autoreport` (
  `id` varchar(32) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `synopsis` varchar(74) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `package` varchar(17) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `fun` varchar(28) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `params` text COLLATE utf8mb4_danish_ci,
  `owner` varchar(14) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `email` varchar(48) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `organization` varchar(22) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `terminateDate` varchar(10) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `interval` varchar(7) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `intervalName` varchar(11) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `runDayOfYear` text COLLATE utf8mb4_danish_ci,
  `type` varchar(12) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `ownerName` varchar(26) COLLATE utf8mb4_danish_ci DEFAULT NULL,
  `startDate` varchar(10) COLLATE utf8mb4_danish_ci DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_danish_ci;
