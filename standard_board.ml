open Property
open Game

let go = create_unbuyable_card "Pass Go" "go" 0

let med_ave =
  create_buyable_card "Mediterranean Avenue" "brown"
    [| 2; 10; 30; 90; 160; 250 |]
    60 50

let community_chest_one =
  create_unbuyable_card "Community Chest No. 1" "community chest" 0

let baltic_ave =
  create_buyable_card "Baltic Avenue" "brown"
    [| 4; 20; 60; 180; 320; 450 |]
    60 50

let income_tax = create_unbuyable_card "Pay $200 Income Tax" "income tax" 200

let reading_rail =
  create_buyable_card "Reading Railroad" "railroad" [| 25 |] 200 0

let oriental_ave =
  create_buyable_card "Oriental Avenue" "light blue"
    [| 6; 30; 90; 270; 400; 550 |]
    100 50

let chance_one = create_unbuyable_card "Chance No. 1" "chance" 0

let vermont_ave =
  create_buyable_card "Vermont Avenue" "light blue"
    [| 6; 30; 90; 270; 400; 550 |]
    100 50

let connecticut_ave =
  create_buyable_card "Connecticut Avenue" "light blue"
    [| 8; 40; 100; 300; 450; 600 |]
    120 50

let jail = create_unbuyable_card "Jail" "jail" 0

let st_charles_pl =
  create_buyable_card "St. Charles Place" "pink"
    [| 10; 50; 150; 450; 625; 750 |]
    140 100

let elec_company =
  create_buyable_card "Electric Company" "utilities" [| 0 |] 150 0

let states_ave =
  create_buyable_card "States Avenue" "pink"
    [| 10; 50; 150; 450; 625; 750 |]
    140 100

let virginia_ave =
  create_buyable_card "Virginia Avenue" "pink"
    [| 10; 50; 150; 450; 625; 750 |]
    140 100

let pennsyl_rail =
  create_buyable_card "Pennsylvania Railroad" "railroad" [| 25 |] 200 0

let st_j_place =
  create_buyable_card "Saint James Place" "orange"
    [| 14; 70; 200; 250; 750; 950 |]
    180 100

let community_chest_two =
  create_unbuyable_card "Community Chest No. 2" "community chest" 0

let tenns_ave =
  create_buyable_card "Tennessee Avenue" "orange"
    [| 14; 70; 200; 250; 750; 950 |]
    180 100

let ny_ave =
  create_buyable_card "New York Avenue" "orange"
    [| 16; 80; 220; 600; 800; 1000 |]
    200 100

let free_park = create_unbuyable_card "Free Parking" "free parking" 0

let ktcy_ave =
  create_buyable_card "Kentucky Avenue" "red"
    [| 18; 90; 250; 700; 875; 1050 |]
    220 150

let chance_two = create_unbuyable_card "Chance No. 2" "chance" 0

let indi_ave =
  create_buyable_card "Indiana Avenue" "red"
    [| 18; 90; 250; 700; 875; 1050 |]
    220 150

let illi_ave =
  create_buyable_card "Illinois Avenue" "red"
    [| 20; 100; 300; 750; 925; 1100 |]
    240 150

let bo_rail = create_buyable_card "B & O Railroad" "railroad" [| 25 |] 200 0

let atlc_ave =
  create_buyable_card "Atlantic Avenue" "yellow"
    [| 22; 110; 330; 800; 975; 1150 |]
    260 150

let vntr_ave =
  create_buyable_card "Ventnor Avenue" "yellow"
    [| 22; 110; 330; 800; 975; 1150 |]
    260 150

let water_works = create_buyable_card "Water Works" "utilities" [| 0 |] 150 0

let mrvn_gdn =
  create_buyable_card "Marvin Gardens" "yellow"
    [| 24; 120; 360; 850; 1025; 1200 |]
    280 150

let go_jail = create_unbuyable_card "Go to Jail" "go to jail" 0

let pafc_ave =
  create_buyable_card "Pacific Avenue" "green"
    [| 26; 130; 390; 900; 1100; 1275 |]
    300 200

let nc_ave =
  create_buyable_card "North Carolina Avenue" "green"
    [| 26; 130; 390; 900; 1100; 1275 |]
    300 200

let community_chest_three =
  create_unbuyable_card "Community Chest No. 3" "community chest" 0

let pennsly_ave =
  create_buyable_card "Pennsylvania Avenue" "green"
    [| 28; 150; 450; 1000; 1200; 1400 |]
    320 200

let st_line_rail =
  create_buyable_card "Short Line Railroad" "railroad" [| 25 |] 200 0

let chance_three = create_unbuyable_card "Chance No. 3" "chance" 0

let pk_place =
  create_buyable_card "Park Place" "dark blue"
    [| 35; 175; 500; 1100; 1300; 1500 |]
    350 200

let luxury_tax = create_unbuyable_card "Pay $100 Luxury Tax" "income tax" 100

let bdwalk =
  create_buyable_card "Boardwalk" "dark blue"
    [| 50; 200; 600; 1400; 1700; 2000 |]
    400 200

let standard_board =
  create_gameboard
    [
      go;
      med_ave;
      community_chest_one;
      baltic_ave;
      income_tax;
      reading_rail;
      oriental_ave;
      chance_one;
      vermont_ave;
      connecticut_ave;
      jail;
      st_charles_pl;
      elec_company;
      states_ave;
      virginia_ave;
      pennsyl_rail;
      st_j_place;
      community_chest_two;
      tenns_ave;
      ny_ave;
      free_park;
      ktcy_ave;
      chance_two;
      indi_ave;
      illi_ave;
      bo_rail;
      atlc_ave;
      vntr_ave;
      water_works;
      mrvn_gdn;
      go_jail;
      pafc_ave;
      nc_ave;
      community_chest_three;
      pennsly_ave;
      st_line_rail;
      chance_three;
      pk_place;
      luxury_tax;
      bdwalk;
    ]
