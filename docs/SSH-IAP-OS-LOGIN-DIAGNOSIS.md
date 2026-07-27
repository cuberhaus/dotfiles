# SSH/IAP Remote-SSH Diagnosis

## Summary

When Remote-SSH reports `Permission denied (publickey)` for the `pol` alias, the failure can occur before VS Code contacts the remote machine. The Google Cloud IAP tunnel may be working while the VM still rejects the SSH key for the requested Linux user.

The `pol` alias currently targets:

```ssh_config
Host vm-pro-it-wwhhq-omniisaacsimnx-02-pol
    HostName vm-pro-it-wwhhq-omniisaacsimnx-02
    User pol
    IdentityFile ~/.ssh/google_compute_engine_iap
    IdentitiesOnly yes
```

The existing `ProxyCommand` must remain configured for the IAP tunnel.

## Confirmed failure path

The direct SSH trace establishes the following sequence:

1. The SSH config resolves the alias to `vm-pro-it-wwhhq-omniisaacsimnx-02` and user `pol`.
2. `gcloud compute start-iap-tunnel` starts successfully.
3. The VM responds and SSH key exchange completes.
4. The VM offers only public-key authentication.
5. The VM rejects the offered `google_compute_engine_iap` key.
6. SSH exits with `Permission denied (publickey)`.

Remote-SSH has not reached the remote machine far enough to install or start the VS Code Server. Consequently, `useLocalServer`, `useExecServer`, port forwarding, and `$PLATFORM is undefined` are not the cause of this failure.

## Reproduce outside VS Code

Run the same SSH path directly:

```bash
ssh -vvv -T -o ConnectTimeout=15 vm-pro-it-wwhhq-omniisaacsimnx-02-pol
```

Inspect the effective configuration:

```bash
ssh -G vm-pro-it-wwhhq-omniisaacsimnx-02-pol | \
  grep -Ei '^(user|hostname|proxycommand|identityfile|identitiesonly)'
```

Verify the local public-key fingerprint:

```bash
ssh-keygen -lf ~/.ssh/google_compute_engine_iap.pub
```

If the public-key file is missing, derive it from the private key without printing the private key:

```bash
ssh-keygen -y -f ~/.ssh/google_compute_engine_iap | ssh-keygen -lf -
```

Check keys currently available through the SSH agent:

```bash
ssh-add -l
```

The configured key and the key offered in the failing trace must have the same fingerprint. A matching local fingerprint only proves that the client is offering the expected key; it does not prove that the VM authorizes it.

## Cloud-side finding

OS Login is enabled both on the VM and in project metadata. The active Google identity's OS Login profile maps to:

```text
pol_casacuberta_gil_extern_horse
```

It does not map to the requested Linux username `pol`. The project metadata contains SSH keys for existing external-user accounts, but metadata keys are not the right authorization path when OS Login is enabled.

This explains why changing the SSH alias, VS Code Server settings, or Remote-SSH options cannot solve the `pol` login by itself.

## Remediation

### If the external account is intended

Use the existing external-user alias and make sure the matching public key is registered with the active Google identity's OS Login profile:

```bash
gcloud compute os-login ssh-keys add \
  --key-file="$HOME/.ssh/google_compute_engine_iap.pub"
```

Then connect using the alias whose `User` is `pol_casacuberta_gil_extern_horse`.

### If the Linux username must be `pol`

A Google Cloud administrator must provision the OS Login identity and permissions that map to `pol`, then register the matching public key for that identity. The administrator should verify:

- The Google identity has the appropriate OS Login IAM role.
- The OS Login profile resolves to Linux user `pol`.
- The public key fingerprint is registered for that identity.
- The VM allows that identity to log in.

If the VM is intended to use a locally managed `pol` account instead of OS Login, an administrator must configure the VM's SSH authorization accordingly. That is a remote VM policy change, not an SSH config or VS Code change.

The VM-side authentication logs can identify the final rejection reason:

```bash
sudo journalctl -u ssh
sudo grep sshd /var/log/auth.log
```

## Independent Google Cloud test

This command tests the Google Cloud SSH path without VS Code. Depending on the OS Login configuration, `gcloud compute ssh` may register the specified public key in the active OS Login profile, so treat it as a cloud-state-changing operation:

```bash
gcloud compute ssh vm-pro-it-wwhhq-omniisaacsimnx-02 \
  --zone=europe-west4-b \
  --project=pro-it-wwhhq-nvidiaaiomni-01 \
  --tunnel-through-iap \
  --ssh-key-file="$HOME/.ssh/google_compute_engine_iap"
```

If this fails with `Permission denied (publickey)`, the remaining issue is remote SSH authorization, OS Login identity mapping, IAM permission, or key registration. It is not a VS Code Server installation issue.

## Working with `/home/pol` through the external account

The working Remote-SSH host authenticates as `pol_casacuberta_gil_extern_horse`, while the desired workspace belongs to `pol`. SSH authentication and filesystem access are separate concerns.

The external account can use the working host and open `/home/pol` in VS Code only after the VM grants it filesystem access. The current workaround uses ACLs on the VM to grant the external account read, write, and traverse access to `/home/pol`, including inherited access for new files:

```bash
sudo setfacl -R -m u:pol_casacuberta_gil_extern_horse:rwX /home/pol
sudo find /home/pol -type d \
  -exec setfacl -m d:u:pol_casacuberta_gil_extern_horse:rwx {} +
```

This grants access to the entire home directory, including private files such as `.ssh`, shell history, and application data. Granting access only to a dedicated project directory is safer.
