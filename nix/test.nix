{ hackage-server, hackage-server-with-tests, pkgs, ... }:

pkgs.testers.runNixOSTest {
  name = "hackage-server";

  containers.machine = { ... }: {
    imports = [ ./nixos-module.nix ];

    services.hackage-server = {
      enable = true;
      package = hackage-server;
      baseUri = "http://localhost:8080";
      userContentUri = "http://localhost:8080";
      requiredBaseHostHeader = "localhost:8080";
      port = 8080;
    };

    environment.systemPackages = [ hackage-server-with-tests ];
  };

  testScript = ''
    machine.start()
    machine.wait_for_unit("hackage-server.socket")

    # Trigger socket activation and wait for the service to come up
    machine.wait_for_open_port(8080)

    # Run the Haskell FeatureTest against the running server
    print(machine.succeed(
        "HACKAGE_TEST_URL=http://localhost:8080 FeatureTest 2>&1"
    ))
  '';
}
