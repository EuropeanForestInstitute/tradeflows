-- Database structure of the validated comext data
--
-- To load this table in the database, use the R function:
--     tradeflows::createdbstructure(sqlfile = "val_comext.sql", dbname = "test")
--

--
-- Table structure for table `val_comext_product`
--
DROP TABLE IF EXISTS `val_comext_product`;
CREATE TABLE `val_comext_product` (
  `productcode` int DEFAULT NULL,
  `productdescription` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `productcode` (`productcode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `val_comext_reporter`
--
DROP TABLE IF EXISTS `val_comext_reporter`;
CREATE TABLE `val_comext_reporter` (
  `reportercode` int DEFAULT NULL,
  `reporter` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `reportercode` (`reportercode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


--
-- Table structure for table `val_comext_partner`
--
DROP TABLE IF EXISTS `val_comext_partner`;
CREATE TABLE `val_comext_partner` (
  `partnercode` int DEFAULT NULL,
  `partner` text COLLATE utf8_unicode_ci,
  UNIQUE KEY `partnercode` (`partnercode`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;



