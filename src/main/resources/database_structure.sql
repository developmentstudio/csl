-- Use at least MySQL server version 5.6.4 or higher.
-- - Earlier versions truncates milliseconds from time and do not allow datetime(3) as syntax.

CREATE TABLE `raw_result_set` (
    `_index` varchar(32) COLLATE utf8_bin NOT NULL,
    `_type` varchar(32) COLLATE utf8_bin NOT NULL,
    `_id` varchar(32) COLLATE utf8_bin NOT NULL,
    `relation` text COLLATE utf8_bin NOT NULL,
    `timestamp` datetime(3) NOT NULL,
    `body` text NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
ALTER TABLE `raw_result_set` ADD PRIMARY KEY (`_id`);

CREATE TABLE `document_label` (
    `_id` varchar(32) COLLATE utf8_bin NOT NULL,
    `variable_name` varchar(32) COLLATE utf8_bin NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_bin;
ALTER TABLE `document_label` ADD PRIMARY KEY (`_id`,`variable_name`);
