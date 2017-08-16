-- Database structure of the dummy data
--
-- To load this table in the database, use the R function:
-- tradeflows::createdbstructure("dummytables.sql", dbname = "test", sqlfolder = ".")
--


--
-- Table structure for table `raw_dummy_code`
--
DROP TABLE IF EXISTS `raw_dummy_code`;
CREATE TABLE `raw_dummy_code` (
  `code` int DEFAULT NULL,
  `description` text COLLATE utf8_unicode_ci,
  `datestart` date DEFAULT NULL,
  `dateend` date DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `raw_dummy_product`
--
DROP TABLE IF EXISTS `raw_dummy_product`;
CREATE TABLE `raw_dummy_product` (
  `productcode` int DEFAULT NULL,
  `productdescription` text COLLATE utf8_unicode_ci,
  `datestart` date DEFAULT NULL,
  `dateend` date DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `raw_dummy_reporter`
--
DROP TABLE IF EXISTS `raw_dummy_reporter`;
CREATE TABLE `raw_dummy_reporter` (
  `reportercode` int DEFAULT NULL,
  `reporter` text COLLATE utf8_unicode_ci,
  `datestart` date DEFAULT NULL,
  `dateend` date DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `raw_dummy_partner`
--
DROP TABLE IF EXISTS `raw_dummy_partner`;
CREATE TABLE `raw_dummy_partner` (
  `partnercode` int DEFAULT NULL,
  `partner` text COLLATE utf8_unicode_ci,
  `datestart` date DEFAULT NULL,
  `dateend` date DEFAULT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `val_dummy_code`
--
DROP TABLE IF EXISTS `val_dummy_code`;
CREATE TABLE `val_dummy_code` (
  `code` int DEFAULT NULL,
  `description` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `code` (`code`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;



--
-- Table structure for table `val_dummy_product`
--
DROP TABLE IF EXISTS `val_dummy_product`;
CREATE TABLE `val_dummy_product` (
  `productcode` int DEFAULT NULL,
  `productdescription` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `productcode` (`productcode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `val_dummy_reporter`
--
DROP TABLE IF EXISTS `val_dummy_reporter`;
CREATE TABLE `val_dummy_reporter` (
  `reportercode` int DEFAULT NULL,
  `reporter` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `reportercode` (`reportercode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `val_dummy_partner`
--
DROP TABLE IF EXISTS `val_dummy_partner`;
CREATE TABLE `val_dummy_partner` (
  `partnercode` int DEFAULT NULL,
  `partner` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `partnercode` (`partnercode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;



