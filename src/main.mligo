module C = struct
  type storage =
    {
     ledger : (address, nat) big_map;
     admin : address;
     start_time : timestamp;
     start_end : timestamp;
     freeze_period : timestamp;
     beneficiaries : (address, nat) big_map;
     amount_claimable : nat
    }

  type ret = operation list * storage

  module Errors = struct
    let not_admin = "NOT_ADMIN"

    let not_allowed = "NOT_ALLOWED"

    let not_enough_funds = "NOT_ENOUGH_FUNDS"

    let unknown_user = "UNKNOWN_USER"
    end

  [@entry]
let start (amount : nat) (store : storage) : ret =
  let () = assert_with_error ((Tezos.get_sender ()) = store.admin) Errors.not_admin in
  let admin_balance =
    match Big_map.find_opt (Tezos.get_sender ()) store.ledger with
      Some (x) -> x
    | None -> failwith Errors.unknown_user in
  match is_nat(admin_balance - amount) with
    Some new_balance -> 
      let ledger_updated = Big_map.update (Tezos.get_sender ()) (Some new_balance) store.ledger in
      ([],
      {
        store with
        ledger = ledger_updated;
        start_time = Tezos.get_now();
        start_end = Tezos.get_now();
        freeze_period = Tezos.get_now();
        amount_claimable = amount
      })
    | None -> failwith Errors.not_enough_funds

  [@entry]
  let addBeneficiary
    (beneficiary, amountToClaim : address * nat)
    (store : storage)
  : ret =
    let () =
      assert_with_error ((Tezos.get_sender ()) = store.admin) Errors.not_admin in
    let currentTime = Tezos.get_now() in
    let () =
      assert_with_error (currentTime <= store.freeze_period) Errors.not_allowed in
    let canAddBeneficiary = (store.amount_claimable >= amountToClaim) in
    let () = if not canAddBeneficiary then failwith Errors.not_allowed else () in
    let beneficiaries_updated =
      Big_map.update beneficiary (Some (amountToClaim)) store.beneficiaries in
    [],
    {
      store with
        beneficiaries = beneficiaries_updated;
        amount_claimable = abs(store.amount_claimable - amountToClaim)
    }

  [@entry]
  let claim (from_ : address) (store : storage) : ret =
    let amountToClaim =
      match Big_map.find_opt from_ store.beneficiaries with
        Some (x) -> x
      | None -> failwith Errors.unknown_user in
    let currentTime = Tezos.get_now() in
    let () =
      assert_with_error (currentTime > store.freeze_period) Errors.not_allowed in
    let () =
      assert_with_error (currentTime <= store.start_end) Errors.not_allowed in
    let ledger_updated =
      match Big_map.find_opt from_ store.ledger with
        Some (x) -> Big_map.update from_ (Some (x + amountToClaim)) store.ledger
      | None -> failwith Errors.unknown_user in // Ajustez selon la logique voulue pour les nouveaux bénéficiaires.
    let beneficiaries_updated = Big_map.update from_ None store.beneficiaries in
    [],
    {
      store with
        ledger = ledger_updated;
        beneficiaries = beneficiaries_updated;
        amount_claimable = abs(store.amount_claimable - amountToClaim)
    }

  end
