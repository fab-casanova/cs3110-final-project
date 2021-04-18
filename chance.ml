open Game

type t = { text : string; effect : Game.t -> unit }

let chance_cards =
  [
    { text = "Advance to Go (Collect $200)"; effect = (fun x -> ()) };
    {
      text = "Advance to Illinois Ave—If you pass Go, collect $200";
      effect = (fun x -> ());
    };
    {
      text = "Advance to St. Charles Place – If you pass Go, collect $200";
      effect = (fun x -> ());
    };
    {
      text =
        "Advance token to nearest Utility. If unowned, you may buy it from the \
         Bank. If owned, throw dice and pay owner a total ten times the amount \
         thrown.";
      effect = (fun x -> ());
    };
    {
      text =
        "Advance token to the nearest Railroad and pay owner twice the rental \
         to which they are otherwise entitled. If Railroad is unowned, you may \
         buy it from the Bank.";
      effect = (fun x -> ());
    };
    { text = "Bank pays you dividend of $50"; effect = (fun x -> ()) };
    { text = "Get Out of Jail Free"; effect = (fun x -> ()) };
    { text = "Go Back 3 Spaces"; effect = (fun x -> ()) };
    {
      text =
        "Go to Jail–Go directly to Jail–Do not pass Go, do not collect $200";
      effect = (fun x -> ());
    };
    {
      text =
        "Make general repairs on all your property–For each house pay \
         $25–For each hotel $100";
      effect = (fun x -> ());
    };
    { text = "Pay poor tax of $15"; effect = (fun x -> ()) };
    {
      text = "Take a trip to Reading Railroad–If you pass Go, collect $200";
      effect = (fun x -> ());
    };
    {
      text = "Take a walk on the Boardwalk–Advance token to Boardwalk";
      effect = (fun x -> ());
    };
    {
      text = "You have been elected Chairman of the Board–Pay each player $50";
      effect = (fun x -> ());
    };
    {
      text = "Your building and loan matures—Collect $150";
      effect = (fun x -> ());
    };
    {
      text = "You have won a crossword competition—Collect $100";
      effect = (fun x -> ());
    };
  ]
