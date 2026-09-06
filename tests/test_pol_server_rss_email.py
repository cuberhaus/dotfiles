import importlib.util
from importlib.machinery import SourceFileLoader
import io
import json
import os
import stat
import tempfile
import unittest
from pathlib import Path
from unittest import mock


REPO_ROOT = Path(__file__).resolve().parents[1]
COMMAND_PATH = REPO_ROOT / "server" / "pol-server" / "rss-email"


def load_command():
    loader = SourceFileLoader("pol_server_rss_email", str(COMMAND_PATH))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class RssEmailTests(unittest.TestCase):
    def test_default_credential_path_is_absolute_system_path(self):
        rss_email = load_command()

        with mock.patch.dict(os.environ, {}, clear=True):
            credential_path = rss_email.configured_credential_path()

        self.assertEqual(
            Path("/etc/cuberhaus/rss-email.json"), credential_path
        )

    def test_first_run_records_baseline_without_sending_email(self):
        rss_email = load_command()
        feed = b"""<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0"><channel>
  <item><title>Newest game</title><link>https://example.test/new</link>
    <guid>new-guid</guid><description>Newest offer</description></item>
  <item><title>Older game</title><link>https://example.test/old</link>
    <guid>old-guid</guid><description>Older offer</description></item>
</channel></rss>"""

        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            credentials = root / "credentials"
            state = root / "state"
            credentials.mkdir()
            (credentials / "rss-email.json").write_text(
                json.dumps(
                    {
                        "sender": "sender@example.test",
                        "recipient": "recipient@example.test",
                        "app_password": "test-secret",
                    }
                ),
                encoding="utf-8",
            )

            environment = {
                "CREDENTIALS_DIRECTORY": str(credentials),
                "STATE_DIRECTORY": str(state),
                "RSS_EMAIL_FEED_URL": "https://feed.example.test/rss",
            }
            response = mock.MagicMock()
            response.__enter__.return_value.read.return_value = feed
            smtp = mock.MagicMock()

            with mock.patch.dict(os.environ, environment, clear=False), mock.patch(
                "urllib.request.urlopen", return_value=response
            ), mock.patch("smtplib.SMTP", smtp), mock.patch(
                "sys.stdout", new_callable=io.StringIO
            ) as stdout:
                exit_code = rss_email.main(["--run"])

            self.assertEqual(0, exit_code)
            self.assertFalse(smtp.called)
            self.assertEqual(
                ["new-guid", "old-guid"],
                json.loads((state / "seen-guids.json").read_text(encoding="utf-8")),
            )
            self.assertIn("Baseline recorded: 2 feed items", stdout.getvalue())

    def test_new_item_is_emailed_once_and_then_recorded(self):
        rss_email = load_command()
        feed = b"""<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0"><channel>
  <item><title>Epic | New game</title><link>https://example.test/new</link>
    <guid>new-guid</guid><description>&lt;b&gt;Free today&lt;/b&gt;</description>
    <pubDate>Fri, 04 Sep 2026 15:53:59 +0000</pubDate></item>
  <item><title>Older game</title><link>https://example.test/old</link>
    <guid>old-guid</guid><description>Older offer</description></item>
</channel></rss>"""

        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            credentials = root / "credentials"
            state = root / "state"
            credentials.mkdir()
            state.mkdir()
            (credentials / "rss-email.json").write_text(
                json.dumps(
                    {
                        "sender": "sender@example.test",
                        "recipient": "recipient@example.test",
                        "app_password": "test-secret",
                    }
                ),
                encoding="utf-8",
            )
            (state / "seen-guids.json").write_text(
                json.dumps(["old-guid"]), encoding="utf-8"
            )

            environment = {
                "CREDENTIALS_DIRECTORY": str(credentials),
                "STATE_DIRECTORY": str(state),
                "RSS_EMAIL_FEED_URL": "https://feed.example.test/rss",
            }
            response = mock.MagicMock()
            response.__enter__.return_value.read.return_value = feed
            smtp = mock.MagicMock()
            smtp_connection = smtp.return_value.__enter__.return_value

            with mock.patch.dict(os.environ, environment, clear=False), mock.patch(
                "urllib.request.urlopen", return_value=response
            ), mock.patch("smtplib.SMTP", smtp):
                first_exit_code = rss_email.main(["--run"])
                second_exit_code = rss_email.main(["--run"])

            self.assertEqual(0, first_exit_code)
            self.assertEqual(0, second_exit_code)
            smtp.assert_called_once_with("smtp.gmail.com", 587, timeout=30)
            smtp_connection.starttls.assert_called_once()
            smtp_connection.login.assert_called_once_with(
                "sender@example.test", "test-secret"
            )
            smtp_connection.send_message.assert_called_once()
            message = smtp_connection.send_message.call_args.args[0]
            self.assertEqual("Epic | New game", message["Subject"])
            self.assertEqual("sender@example.test", message["From"])
            self.assertEqual("recipient@example.test", message["To"])
            self.assertIn("Free today", message.get_content())
            self.assertIn("https://example.test/new", message.get_content())
            self.assertEqual(
                ["old-guid", "new-guid"],
                json.loads((state / "seen-guids.json").read_text(encoding="utf-8")),
            )

    def test_failed_delivery_remains_pending_for_retry(self):
        rss_email = load_command()
        feed = b"""<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0"><channel>
  <item><title>Newest game</title><link>https://example.test/two</link>
    <guid>new-guid-2</guid><description>Second offer</description></item>
  <item><title>First new game</title><link>https://example.test/one</link>
    <guid>new-guid-1</guid><description>First offer</description></item>
  <item><title>Baseline game</title><link>https://example.test/base</link>
    <guid>baseline-guid</guid><description>Old offer</description></item>
</channel></rss>"""

        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            credentials = root / "credentials"
            state = root / "state"
            credentials.mkdir()
            state.mkdir()
            (credentials / "rss-email.json").write_text(
                json.dumps(
                    {
                        "sender": "sender@example.test",
                        "recipient": "recipient@example.test",
                        "app_password": "test-secret",
                    }
                ),
                encoding="utf-8",
            )
            (state / "seen-guids.json").write_text(
                json.dumps(["baseline-guid"]), encoding="utf-8"
            )

            environment = {
                "CREDENTIALS_DIRECTORY": str(credentials),
                "STATE_DIRECTORY": str(state),
                "RSS_EMAIL_FEED_URL": "https://feed.example.test/rss",
            }
            response = mock.MagicMock()
            response.__enter__.return_value.read.return_value = feed
            smtp = mock.MagicMock()
            smtp_connection = smtp.return_value.__enter__.return_value
            smtp_connection.send_message.side_effect = [
                None,
                rss_email.smtplib.SMTPException("temporary failure"),
            ]

            with mock.patch.dict(os.environ, environment, clear=False), mock.patch(
                "urllib.request.urlopen", return_value=response
            ), mock.patch("smtplib.SMTP", smtp), mock.patch(
                "sys.stderr", new_callable=io.StringIO
            ) as stderr:
                exit_code = rss_email.main(["--run"])

            self.assertEqual(1, exit_code)
            self.assertEqual(2, smtp_connection.send_message.call_count)
            self.assertEqual(
                ["baseline-guid", "new-guid-1"],
                json.loads((state / "seen-guids.json").read_text(encoding="utf-8")),
            )
            self.assertIn("RSS email failed: temporary failure", stderr.getvalue())

    def test_test_email_sends_without_changing_feed_state(self):
        rss_email = load_command()

        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            credentials = root / "credentials"
            state = root / "state"
            credentials.mkdir()
            state.mkdir()
            state_path = state / "seen-guids.json"
            state_path.write_text(json.dumps(["baseline-guid"]), encoding="utf-8")
            (credentials / "rss-email.json").write_text(
                json.dumps(
                    {
                        "sender": "sender@example.test",
                        "recipient": "recipient@example.test",
                        "app_password": "test-secret",
                    }
                ),
                encoding="utf-8",
            )
            environment = {
                "CREDENTIALS_DIRECTORY": str(credentials),
                "STATE_DIRECTORY": str(state),
            }
            smtp = mock.MagicMock()
            smtp_connection = smtp.return_value.__enter__.return_value

            with mock.patch.dict(os.environ, environment, clear=False), mock.patch(
                "smtplib.SMTP", smtp
            ), mock.patch("sys.stdout", new_callable=io.StringIO) as stdout:
                exit_code = rss_email.main(["--test-email"])

            self.assertEqual(0, exit_code)
            smtp_connection.send_message.assert_called_once()
            message = smtp_connection.send_message.call_args.args[0]
            self.assertEqual("[pol-server] RSS email delivery test", message["Subject"])
            self.assertEqual("recipient@example.test", message["To"])
            self.assertEqual(
                ["baseline-guid"],
                json.loads(state_path.read_text(encoding="utf-8")),
            )
            self.assertIn("Test email sent to recipient@example.test", stdout.getvalue())

    def test_wd_report_email_contains_report_without_changing_feed_state(self):
        rss_email = load_command()

        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            credentials = root / "credentials"
            state = root / "state"
            credentials.mkdir()
            state.mkdir()
            state_path = state / "seen-guids.json"
            state_path.write_text(json.dumps(["baseline-guid"]), encoding="utf-8")
            (credentials / "rss-email.json").write_text(
                json.dumps(
                    {
                        "sender": "sender@example.test",
                        "recipient": "recipient@example.test",
                        "app_password": "test-secret",
                    }
                ),
                encoding="utf-8",
            )
            environment = {
                "CREDENTIALS_DIRECTORY": str(credentials),
                "STATE_DIRECTORY": str(state),
            }
            report = "SMART overall-health self-assessment test result: PASSED\n# 1 Extended offline Completed without error\n"
            report_command = mock.MagicMock(
                returncode=0,
                stdout=report,
                stderr="",
            )
            smtp = mock.MagicMock()
            smtp_connection = smtp.return_value.__enter__.return_value

            with mock.patch.dict(os.environ, environment, clear=False), mock.patch(
                "subprocess.run", return_value=report_command
            ) as run_command, mock.patch("smtplib.SMTP", smtp), mock.patch(
                "sys.stdout", new_callable=io.StringIO
            ) as stdout:
                exit_code = rss_email.main(["--send-wd-report"])

            self.assertEqual(0, exit_code)
            run_command.assert_called_once_with(
                [
                    "/usr/local/sbin/pol-server-hardware",
                    "--report-disk",
                    "wd-backup",
                ],
                capture_output=True,
                check=False,
                text=True,
            )
            smtp_connection.send_message.assert_called_once()
            message = smtp_connection.send_message.call_args.args[0]
            self.assertEqual("[pol-server] WD SMART post-test report", message["Subject"])
            self.assertEqual("recipient@example.test", message["To"])
            self.assertIn(report, message.get_content())
            self.assertEqual(
                ["baseline-guid"],
                json.loads(state_path.read_text(encoding="utf-8")),
            )
            self.assertIn(
                "WD SMART report sent to recipient@example.test", stdout.getvalue()
            )

    def test_configure_validates_and_stores_root_only_gmail_credential(self):
        rss_email = load_command()

        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            environment = {
                "POL_SERVER_ALLOW_UNPRIVILEGED": "true",
                "POL_SERVER_ROOT": str(root),
            }
            smtp = mock.MagicMock()
            smtp_connection = smtp.return_value.__enter__.return_value
            systemctl = mock.MagicMock()
            answers = io.StringIO(
                "sender@example.test\n\nabcd efgh ijkl mnop\n"
            )

            with mock.patch.dict(os.environ, environment, clear=False), mock.patch(
                "smtplib.SMTP", smtp
            ), mock.patch("subprocess.run", systemctl), mock.patch(
                "sys.stdin", answers
            ), mock.patch("sys.stdout", new_callable=io.StringIO):
                exit_code = rss_email.main(["--configure"])

            credential_path = root / "etc" / "cuberhaus" / "rss-email.json"
            self.assertEqual(0, exit_code)
            self.assertEqual(
                {
                    "sender": "sender@example.test",
                    "recipient": "sender@example.test",
                    "app_password": "abcdefghijklmnop",
                },
                json.loads(credential_path.read_text(encoding="utf-8")),
            )
            self.assertEqual(0o600, credential_path.stat().st_mode & 0o777)
            smtp.assert_called_once_with("smtp.gmail.com", 587, timeout=30)
            smtp_connection.starttls.assert_called_once()
            smtp_connection.login.assert_called_once_with(
                "sender@example.test", "abcdefghijklmnop"
            )
            smtp_connection.send_message.assert_not_called()
            systemctl.assert_has_calls(
                [
                    mock.call(["systemctl", "daemon-reload"], check=True),
                    mock.call(
                        ["systemctl", "start", "pol-server-rss-email.service"],
                        check=True,
                    ),
                    mock.call(
                        [
                            "systemctl",
                            "enable",
                            "--now",
                            "pol-server-rss-email.timer",
                        ],
                        check=True,
                    ),
                ]
            )

    def test_check_reuses_valid_credential_and_verifies_timer_and_smtp(self):
        rss_email = load_command()

        with tempfile.TemporaryDirectory() as temporary_directory:
            root = Path(temporary_directory)
            credential_path = root / "etc" / "cuberhaus" / "rss-email.json"
            credential_path.parent.mkdir(parents=True)
            credential_path.write_text(
                json.dumps(
                    {
                        "sender": "sender@example.test",
                        "recipient": "recipient@example.test",
                        "app_password": "test-secret",
                    }
                ),
                encoding="utf-8",
            )
            credential_path.chmod(stat.S_IRUSR | stat.S_IWUSR)
            environment = {
                "POL_SERVER_ALLOW_UNPRIVILEGED": "true",
                "POL_SERVER_ROOT": str(root),
            }
            smtp = mock.MagicMock()

            with mock.patch.dict(os.environ, environment, clear=False), mock.patch(
                "smtplib.SMTP", smtp
            ), mock.patch("subprocess.run") as systemctl, mock.patch(
                "sys.stdout", new_callable=io.StringIO
            ) as stdout:
                exit_code = rss_email.main(["--check"])

            self.assertEqual(0, exit_code)
            smtp.return_value.__enter__.return_value.login.assert_called_once_with(
                "sender@example.test", "test-secret"
            )
            systemctl.assert_has_calls(
                [
                    mock.call(
                        ["systemctl", "is-active", "--quiet", "pol-server-rss-email.timer"],
                        check=True,
                    ),
                    mock.call(
                        ["systemctl", "is-enabled", "--quiet", "pol-server-rss-email.timer"],
                        check=True,
                    ),
                ]
            )
            self.assertIn("RSS email configuration healthy", stdout.getvalue())

    def test_check_rejects_missing_credential_without_prompting(self):
        rss_email = load_command()

        with tempfile.TemporaryDirectory() as temporary_directory:
            environment = {
                "POL_SERVER_ALLOW_UNPRIVILEGED": "true",
                "POL_SERVER_ROOT": temporary_directory,
            }
            with mock.patch.dict(os.environ, environment, clear=False), mock.patch(
                "sys.stderr", new_callable=io.StringIO
            ) as stderr:
                exit_code = rss_email.main(["--check"])

            self.assertEqual(1, exit_code)
            self.assertIn("RSS email failed", stderr.getvalue())


if __name__ == "__main__":
    unittest.main()