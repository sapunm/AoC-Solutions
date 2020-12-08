
#[derive(Clone)]
pub enum OpCode {
    Acc,
    Jmp,
    Nop
}

pub type Instruction = (OpCode, i64);
pub type DataType = Vec<Instruction>;
pub type ResultType = i64;

use OpCode::*;

lazy_static! {
    pub static ref DATA: DataType = vec![
        (Acc, 18),
        (Nop, 222),
        (Acc, -16),
        (Acc, 28),
        (Jmp, 475),
        (Acc, -6),
        (Jmp, 584),
        (Acc, -12),
        (Acc, -8),
        (Jmp, 554),
        (Acc, -9),
        (Acc, 12),
        (Acc, -16),
        (Acc, 27),
        (Jmp, 336),
        (Acc, -4),
        (Jmp, 214),
        (Acc, 38),
        (Jmp, 61),
        (Acc, 3),
        (Acc, 28),
        (Acc, 5),
        (Acc, -19),
        (Jmp, 584),
        (Nop, 206),
        (Jmp, 506),
        (Acc, 36),
        (Jmp, 133),
        (Acc, 20),
        (Acc, 43),
        (Acc, -18),
        (Jmp, 409),
        (Acc, 24),
        (Jmp, 131),
        (Acc, -12),
        (Acc, 7),
        (Acc, 7),
        (Jmp, 454),
        (Acc, 37),
        (Acc, -6),
        (Nop, 558),
        (Acc, 31),
        (Jmp, 124),
        (Acc, -15),
        (Nop, 201),
        (Acc, -7),
        (Jmp, 297),
        (Acc, 3),
        (Nop, 517),
        (Jmp, 221),
        (Jmp, 211),
        (Acc, 28),
        (Acc, 35),
        (Jmp, 5),
        (Acc, 31),
        (Nop, 325),
        (Acc, -15),
        (Jmp, 116),
        (Jmp, 1),
        (Nop, 333),
        (Acc, -2),
        (Acc, -5),
        (Jmp, 138),
        (Acc, 19),
        (Acc, 9),
        (Jmp, 180),
        (Acc, 18),
        (Jmp, 228),
        (Jmp, 495),
        (Jmp, 382),
        (Acc, 20),
        (Nop, 414),
        (Nop, 139),
        (Acc, 33),
        (Jmp, 171),
        (Acc, -10),
        (Jmp, 41),
        (Acc, -2),
        (Jmp, 80),
        (Acc, 20),
        (Nop, 451),
        (Acc, 2),
        (Acc, 24),
        (Jmp, 102),
        (Acc, 1),
        (Acc, -11),
        (Acc, 9),
        (Acc, 38),
        (Jmp, -73),
        (Acc, 17),
        (Acc, 16),
        (Acc, 12),
        (Acc, 43),
        (Jmp, 168),
        (Jmp, 286),
        (Acc, 6),
        (Acc, 6),
        (Jmp, 271),
        (Acc, -17),
        (Acc, -5),
        (Acc, 1),
        (Jmp, -50),
        (Acc, -9),
        (Acc, 6),
        (Acc, -2),
        (Acc, 33),
        (Jmp, 385),
        (Acc, 18),
        (Acc, 24),
        (Jmp, 370),
        (Acc, -5),
        (Acc, 23),
        (Acc, 6),
        (Jmp, 98),
        (Acc, -10),
        (Acc, -16),
        (Jmp, 329),
        (Nop, 41),
        (Jmp, 463),
        (Nop, 224),
        (Acc, 35),
        (Jmp, 345),
        (Acc, 34),
        (Acc, -18),
        (Acc, 5),
        (Jmp, 177),
        (Nop, -57),
        (Nop, -80),
        (Acc, 20),
        (Jmp, -12),
        (Acc, 24),
        (Acc, 39),
        (Jmp, 363),
        (Jmp, 253),
        (Acc, -14),
        (Acc, 0),
        (Acc, 22),
        (Jmp, 118),
        (Acc, 43),
        (Acc, -2),
        (Jmp, 300),
        (Acc, -14),
        (Acc, 8),
        (Acc, 47),
        (Jmp, 271),
        (Jmp, 420),
        (Acc, 33),
        (Acc, 15),
        (Acc, 20),
        (Acc, 25),
        (Jmp, 84),
        (Acc, 41),
        (Jmp, 420),
        (Acc, 25),
        (Jmp, 238),
        (Jmp, 1),
        (Acc, 14),
        (Jmp, 415),
        (Jmp, 68),
        (Jmp, 262),
        (Acc, 34),
        (Jmp, 346),
        (Acc, 39),
        (Jmp, 56),
        (Jmp, 364),
        (Jmp, -133),
        (Acc, 13),
        (Jmp, 1),
        (Acc, 33),
        (Jmp, 408),
        (Acc, 29),
        (Acc, -4),
        (Jmp, 319),
        (Jmp, 106),
        (Jmp, 228),
        (Acc, -8),
        (Acc, 8),
        (Acc, 22),
        (Jmp, -146),
        (Jmp, 223),
        (Acc, 27),
        (Nop, 191),
        (Acc, 49),
        (Jmp, 331),
        (Jmp, 39),
        (Jmp, -170),
        (Acc, 28),
        (Acc, -6),
        (Acc, 50),
        (Jmp, 268),
        (Acc, 41),
        (Nop, 254),
        (Acc, 28),
        (Jmp, 269),
        (Jmp, 140),
        (Acc, 10),
        (Nop, 131),
        (Acc, 3),
        (Jmp, -40),
        (Jmp, 373),
        (Acc, 47),
        (Jmp, -91),
        (Acc, -19),
        (Jmp, 300),
        (Acc, -2),
        (Jmp, 1),
        (Acc, 44),
        (Acc, -11),
        (Jmp, 306),
        (Acc, 33),
        (Jmp, -15),
        (Acc, 9),
        (Jmp, 1),
        (Jmp, 144),
        (Acc, 40),
        (Nop, 184),
        (Nop, -75),
        (Nop, 228),
        (Jmp, 296),
        (Acc, 22),
        (Nop, 364),
        (Jmp, -214),
        (Jmp, 18),
        (Jmp, 375),
        (Acc, 22),
        (Jmp, -67),
        (Acc, 8),
        (Acc, -17),
        (Jmp, 174),
        (Jmp, -99),
        (Nop, -45),
        (Acc, 7),
        (Jmp, -213),
        (Jmp, -218),
        (Acc, 50),
        (Nop, 52),
        (Nop, 98),
        (Jmp, -142),
        (Acc, 18),
        (Jmp, 252),
        (Acc, 36),
        (Jmp, -194),
        (Acc, 1),
        (Nop, -53),
        (Jmp, -127),
        (Jmp, 327),
        (Acc, 7),
        (Acc, -9),
        (Acc, 39),
        (Nop, -127),
        (Jmp, 84),
        (Jmp, -117),
        (Nop, -29),
        (Acc, 43),
        (Jmp, -216),
        (Acc, 25),
        (Acc, 16),
        (Acc, 40),
        (Nop, 122),
        (Jmp, 140),
        (Jmp, 180),
        (Acc, 42),
        (Acc, -5),
        (Acc, -14),
        (Jmp, -84),
        (Jmp, -31),
        (Acc, 37),
        (Acc, -11),
        (Jmp, -217),
        (Jmp, 210),
        (Jmp, 170),
        (Nop, 301),
        (Jmp, 309),
        (Acc, 6),
        (Jmp, 135),
        (Acc, 6),
        (Nop, -123),
        (Acc, 17),
        (Jmp, 315),
        (Acc, -1),
        (Nop, -46),
        (Nop, -58),
        (Nop, -59),
        (Jmp, 202),
        (Acc, 48),
        (Acc, 38),
        (Jmp, 86),
        (Acc, -4),
        (Acc, 33),
        (Acc, 28),
        (Jmp, -50),
        (Nop, 43),
        (Acc, 38),
        (Acc, 13),
        (Jmp, 33),
        (Acc, 4),
        (Acc, 6),
        (Jmp, -78),
        (Acc, 22),
        (Acc, 7),
        (Acc, -9),
        (Jmp, -56),
        (Acc, 30),
        (Nop, 54),
        (Nop, -81),
        (Nop, 198),
        (Jmp, 252),
        (Jmp, 1),
        (Acc, 6),
        (Acc, -10),
        (Acc, 29),
        (Jmp, -242),
        (Jmp, 1),
        (Acc, 42),
        (Acc, 34),
        (Acc, 22),
        (Jmp, 231),
        (Acc, 29),
        (Acc, -10),
        (Jmp, -161),
        (Acc, 37),
        (Acc, 9),
        (Jmp, -77),
        (Acc, -15),
        (Acc, 32),
        (Acc, 32),
        (Jmp, -6),
        (Acc, 0),
        (Nop, -124),
        (Nop, 174),
        (Jmp, 20),
        (Acc, 45),
        (Acc, 24),
        (Jmp, -13),
        (Acc, 6),
        (Acc, -10),
        (Acc, 23),
        (Acc, -15),
        (Jmp, 34),
        (Acc, 5),
        (Acc, 38),
        (Acc, 42),
        (Jmp, -116),
        (Acc, 0),
        (Acc, 8),
        (Jmp, -243),
        (Acc, -18),
        (Acc, 25),
        (Acc, 1),
        (Jmp, 158),
        (Nop, 65),
        (Jmp, 1),
        (Jmp, 151),
        (Acc, 12),
        (Acc, 12),
        (Jmp, 1),
        (Jmp, -305),
        (Jmp, 29),
        (Jmp, -263),
        (Acc, 33),
        (Jmp, 1),
        (Nop, 142),
        (Jmp, 78),
        (Acc, 41),
        (Nop, -141),
        (Acc, -9),
        (Acc, 5),
        (Jmp, -245),
        (Jmp, 41),
        (Acc, 16),
        (Nop, -83),
        (Jmp, -28),
        (Nop, -149),
        (Acc, 38),
        (Jmp, -15),
        (Acc, 7),
        (Nop, -329),
        (Acc, 5),
        (Acc, 21),
        (Jmp, -7),
        (Acc, -19),
        (Jmp, -38),
        (Acc, 5),
        (Acc, 3),
        (Acc, 10),
        (Jmp, -181),
        (Jmp, -240),
        (Acc, 19),
        (Acc, 15),
        (Acc, 31),
        (Acc, -11),
        (Jmp, -340),
        (Acc, 12),
        (Acc, 46),
        (Jmp, 127),
        (Acc, 12),
        (Acc, 31),
        (Acc, 30),
        (Jmp, -158),
        (Acc, -10),
        (Jmp, -374),
        (Jmp, 50),
        (Acc, 43),
        (Nop, 42),
        (Acc, 19),
        (Jmp, -232),
        (Acc, -14),
        (Acc, -4),
        (Jmp, 95),
        (Acc, 23),
        (Acc, 49),
        (Acc, 31),
        (Nop, -139),
        (Jmp, -272),
        (Jmp, -141),
        (Acc, 26),
        (Acc, -8),
        (Jmp, 173),
        (Nop, 145),
        (Nop, 133),
        (Jmp, 164),
        (Acc, 7),
        (Jmp, 23),
        (Acc, -4),
        (Acc, 48),
        (Jmp, -138),
        (Acc, 4),
        (Jmp, -389),
        (Nop, 156),
        (Acc, 44),
        (Acc, 40),
        (Jmp, 146),
        (Nop, -247),
        (Acc, 44),
        (Jmp, 1),
        (Acc, 28),
        (Jmp, 95),
        (Acc, 13),
        (Acc, 2),
        (Jmp, -254),
        (Acc, 24),
        (Jmp, 122),
        (Acc, 39),
        (Acc, 0),
        (Jmp, -12),
        (Jmp, -179),
        (Nop, -44),
        (Nop, 100),
        (Acc, -19),
        (Nop, -47),
        (Jmp, -107),
        (Acc, 32),
        (Acc, 33),
        (Acc, 42),
        (Acc, 6),
        (Jmp, -366),
        (Jmp, -122),
        (Acc, 2),
        (Nop, -443),
        (Nop, 72),
        (Jmp, -381),
        (Jmp, -446),
        (Jmp, -332),
        (Acc, -7),
        (Acc, 45),
        (Jmp, -355),
        (Acc, 27),
        (Acc, -4),
        (Acc, 3),
        (Jmp, 96),
        (Acc, 45),
        (Jmp, -402),
        (Acc, 45),
        (Acc, -3),
        (Acc, 22),
        (Jmp, -141),
        (Acc, 29),
        (Acc, -1),
        (Jmp, 29),
        (Acc, -1),
        (Acc, -10),
        (Jmp, -208),
        (Acc, 6),
        (Nop, -196),
        (Jmp, -218),
        (Acc, -12),
        (Acc, 49),
        (Nop, -137),
        (Jmp, -430),
        (Acc, 21),
        (Jmp, -110),
        (Nop, -287),
        (Acc, -3),
        (Jmp, -42),
        (Jmp, -487),
        (Acc, -16),
        (Acc, -1),
        (Acc, 7),
        (Acc, 39),
        (Jmp, -119),
        (Jmp, 1),
        (Acc, 9),
        (Jmp, -23),
        (Acc, 27),
        (Jmp, -300),
        (Acc, 12),
        (Jmp, -440),
        (Acc, 2),
        (Acc, 38),
        (Acc, 12),
        (Jmp, -84),
        (Acc, 25),
        (Acc, -14),
        (Jmp, -418),
        (Acc, -15),
        (Acc, 48),
        (Jmp, 1),
        (Nop, -383),
        (Jmp, -365),
        (Acc, 47),
        (Jmp, -193),
        (Acc, 23),
        (Jmp, -235),
        (Jmp, 1),
        (Acc, -4),
        (Acc, 35),
        (Nop, -64),
        (Jmp, -87),
        (Acc, 32),
        (Jmp, -339),
        (Jmp, -479),
        (Acc, -4),
        (Acc, 32),
        (Acc, -10),
        (Jmp, -77),
        (Acc, 0),
        (Acc, 47),
        (Acc, 41),
        (Jmp, -308),
        (Acc, -8),
        (Acc, -9),
        (Jmp, -229),
        (Acc, -14),
        (Acc, 24),
        (Nop, -380),
        (Acc, 49),
        (Jmp, -174),
        (Acc, -11),
        (Nop, -69),
        (Jmp, 3),
        (Acc, -14),
        (Jmp, -89),
        (Jmp, -301),
        (Acc, 46),
        (Acc, 8),
        (Nop, -156),
        (Acc, 44),
        (Jmp, 1),
        (Jmp, 26),
        (Acc, 17),
        (Acc, 23),
        (Acc, 6),
        (Jmp, -4),
        (Jmp, -97),
        (Jmp, -324),
        (Acc, 2),
        (Jmp, -27),
        (Nop, -195),
        (Acc, 3),
        (Acc, -13),
        (Acc, 15),
        (Jmp, -19),
        (Acc, 30),
        (Nop, -318),
        (Jmp, 19),
        (Nop, -72),
        (Jmp, -315),
        (Acc, 4),
        (Nop, 6),
        (Jmp, -384),
        (Jmp, -505),
        (Jmp, -512),
        (Acc, 33),
        (Jmp, -168),
        (Jmp, -443),
        (Nop, -519),
        (Acc, 7),
        (Acc, 41),
        (Acc, 15),
        (Jmp, -269),
        (Nop, -539),
        (Jmp, -416),
        (Jmp, -326),
        (Nop, -221),
        (Acc, 14),
        (Jmp, -186),
        (Acc, -1),
        (Jmp, -295),
        (Acc, 29),
        (Acc, 43),
        (Nop, -436),
        (Nop, -421),
        (Jmp, -123),
        (Acc, 13),
        (Acc, -11),
        (Acc, 12),
        (Jmp, -155),
        (Acc, 9),
        (Acc, -16),
        (Acc, -15),
        (Nop, -380),
        (Jmp, 1),
    ];
}