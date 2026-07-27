# Remote Home Access

The Remote-SSH connection authenticates as `pol_casacuberta_gil_extern_horse`, while the workspace is under `/home/pol`. The following commands were used on `vm-pro-it-wwhhq-omniisaacsimnx-02` to grant the authenticated account access to the complete `/home/pol` tree.

## Grant access

Run these commands on the VM as an administrator:

```bash
sudo setfacl -R -m u:pol_casacuberta_gil_extern_horse:rwX /home/pol
sudo find /home/pol -type d \
  -exec setfacl -m d:u:pol_casacuberta_gil_extern_horse:rwx {} +
```

The first command grants read, write, and directory-traversal access to existing files and directories. The second adds a default ACL to every directory so new files inherit access.

Ownership remains with `pol`; these commands add an ACL entry for the external account.

## Verify access

Run as `pol_casacuberta_gil_extern_horse`:

```bash
existing_file=$(find /home/pol -maxdepth 2 -type f -print -quit)
test -n "$existing_file" && test -r "$existing_file" && test -w "$existing_file"
getfacl -cp /home/pol | grep 'user:pol_casacuberta_gil_extern_horse:'
```

To verify inheritance without changing project contents:

```bash
temp_dir=$(mktemp -d /home/pol/.acl-check.XXXXXX)
trap 'rm -rf "$temp_dir"' EXIT
touch "$temp_dir/file"
getfacl -cp "$temp_dir/file" | grep 'user:pol_casacuberta_gil_extern_horse:'
```

## Revoke access

To remove both the existing ACL entries and the default ACL entries later:

```bash
sudo setfacl -R -x u:pol_casacuberta_gil_extern_horse /home/pol
sudo find /home/pol -type d \
  -exec setfacl -x d:u:pol_casacuberta_gil_extern_horse {} +
```

## Security warning

This grants the external account access to the entire home directory, including private files such as `.ssh`, shell history, and application data. Grant access to a dedicated project directory instead when full-home access is not required.
