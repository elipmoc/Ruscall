mod cmd_args;

fn main() {
    cmd_args::parse_cmd_args().run();
}
