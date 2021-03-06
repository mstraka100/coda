open Tc;

module StakingSwitch = {
  [@react.component]
  let make = (~pubKey, ~settings) => {
    <div className=Css.(style([color(StyleGuide.Colors.serpentine)]))>
      <input
        type_="checkbox"
        label="staking-switch"
        checked=true
        onChange={_e => Js.log("TODO: Implement stake changing")}
      />
      <span> {ReasonReact.string("Staking")} </span>
      <span className=Css.(style([fontFamily("Menlo")]))>
        {ReasonReact.string({j| ⚡︎ |j})}
      </span>
      <span>
        {ReasonReact.string(
           SettingsRenderer.lookup(settings, pubKey)
           |> Option.withDefault(~default=pubKey |> PublicKey.toString),
         )}
      </span>
    </div>;
  };
};

module ActivityLogButton = {
  [@react.component]
  let make = () => {
    <button>
      <span> {ReasonReact.string("Activity Log")} </span>
      <span className=Css.(style([marginLeft(`rem(1.0))]))>
        {ReasonReact.string({j|⌘L|j})}
      </span>
    </button>;
  };
};

module RightButtons = {
  module PublicKeyButton = {
    [@react.component]
    let make = (~pubKeySelected) => {
      let str = ReasonReact.string("Copy public key");
      // The switch is over the <button> rather than within the onClick because
      // if there exists an onClick the button is no longer disabled (despite
      // disabled being true), and there is no way to give JSX an `option` for
      // the click handler.
      switch (pubKeySelected) {
      | Some(pubKey) =>
        <button
          onClick={_e => {
            let task =
              Bindings.Navigator.Clipboard.writeTextTask(
                PublicKey.toString(pubKey),
              );
            Task.perform(task, ~f=()
              // TODO: Should we toast when this happens? Do we need to handle errors?
              => Js.log("Copied to clipboard"));
          }}
          disabled=false>
          str
        </button>
      | None => <button disabled=true> str </button>
      };
    };
  };

  module SendButton = {
    [@react.component]
    let make = (~settings) => {
      <button
        onClick={_e =>
          Router.navigate({
            path: Route.Path.Send,
            settingsOrError: `Settings(settings),
          })
        }>
        {ReasonReact.string("Send")}
      </button>;
    };
  };

  [@react.component]
  let make = (~pubKeySelected, ~settings) => {
    <div> <PublicKeyButton pubKeySelected /> <SendButton settings /> </div>;
  };
};

[@react.component]
let make = (~stakingKey, ~settings) => {
  <div
    className=Css.(
      style([
        display(`flex),
        justifyContent(`spaceBetween),
        alignItems(`center),
        padding(`rem(2.0)),
      ])
    )>
    <StakingSwitch pubKey=stakingKey settings />
    <ActivityLogButton />
    <RightButtons pubKeySelected=None settings />
  </div>;
};
