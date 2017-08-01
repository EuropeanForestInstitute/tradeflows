--
-- MySQL table structure for the Comext raw data
--
-- To load this table in the database, use the R function:
--     tradeharvester::createdbstructure()
--
-- Loading this structure will erase all data in
-- raw_comext_cn, raw_comext_monthly, etc... tables defined below,
-- but it will not erase data in specific
-- monthly recent (raw_comext_monthly_[year][month])
-- and archived tables (raw_comext_monthly_[year] and raw_comext_yearly_[year]),
-- as these data table are a copy of the tables defined below and have a
-- different name.
--
-- To dump the existing database structure (without data):
-- Provided a password is set in ~/.my.cnf, in the [client] group
-- mysqldump -d tradeflows > tradeflows.sql
--


--
-- Table structure for table `raw_comext_cn`
--
DROP TABLE IF EXISTS `raw_comext_cn`;
CREATE TABLE `raw_comext_cn` (
  `productcode` int DEFAULT NULL,
  `datestart` date DEFAULT NULL,
  `dateend` date DEFAULT NULL,
  `productdescription` text COLLATE utf8_unicode_ci,
  `datestart2` date DEFAULT NULL,
  `dateend2` date DEFAULT NULL,
  KEY `productcode` (`productcode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `raw_comext_reporter`
--
DROP TABLE IF EXISTS `raw_comext_reporter`;
CREATE TABLE `raw_comext_reporter` (
  `reportercode` int DEFAULT NULL,
  `datestart` date DEFAULT NULL,
  `dateend` date DEFAULT NULL,
  `reporter` text COLLATE utf8_unicode_ci,
  `datestart2` date DEFAULT NULL,
  `dateend2` date DEFAULT NULL,
  KEY `reportercode` (`reportercode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `raw_comext_partner`
--
DROP TABLE IF EXISTS `raw_comext_partner`;
CREATE TABLE `raw_comext_partner` (
  `partnercode` int DEFAULT NULL,
  `datestart` date DEFAULT NULL,
  `dateend` date DEFAULT NULL,
  `partner` text COLLATE utf8_unicode_ci,
  `datestart2` date DEFAULT NULL,
  `dateend2` date DEFAULT NULL,
  KEY `partnercode` (`partnercode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `raw_comext_monthly`
--
DROP TABLE IF EXISTS `raw_comext_monthly`;
CREATE TABLE `raw_comext_monthly` (
  `reportercode` int DEFAULT NULL,
  `partnercode` int DEFAULT NULL,
  `productcode` int DEFAULT NULL,
  `flowcode` int DEFAULT NULL,
  `statregime` int DEFAULT NULL,
  `period` int DEFAULT NULL,
  `tradevalue` double DEFAULT NULL,
  `weight` double DEFAULT NULL,
  `quantity` int DEFAULT NULL,
  KEY `productcode` (`productcode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

