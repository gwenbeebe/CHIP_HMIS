------------------------------------------------
----------- calculate client age ---------------
------------------------------------------------

(
	CONVERT(
		int,
		CONVERT(
			char(8),
			Enrollment.[EnrollDate],
			112)
		)
	-
	CONVERT(
		char(8),
		cmClient.[Birthdate],
		112
		)
)
/10000
