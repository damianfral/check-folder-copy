export const showDirectoryPicker = () =>
  window.showDirectoryPicker({ mode: "read" });

export const getDirectoryHandle = (dirHandle) => (name) =>
  dirHandle.getDirectoryHandle(name);

export const getDirectoryHandleName = (dirHandle) => dirHandle.name;
export const getFileHandleName = (dirHandle) => dirHandle.name;

export const getDirectoryContents = (dirHandle) => async () => {
  const directories = [];
  const files = [];

  for await (const entry of dirHandle.values()) {
    if (entry.kind === "directory") directories.push(entry);
    else if (entry.kind === "file") files.push(entry);
  }

  directories.sort((a, b) => a.name.localeCompare(b.name));
  files.sort((a, b) => a.name.localeCompare(b.name));

  return { directories, files };
};

export const fullPath = (dirHandle) => dirHandle.fullPath;

export const getFileHandle = (dirHandle) => (name) =>
  dirHandle.getFileHandle(name);

export const getFileFromHandle = (fileHandle) => () =>
  fileHandle.getFile();

export const getDataTransfer = (ev) => { return ev.dataTransfer }
export const getAsFileSystemHandle = (dt) => dt.getAsFileSystemHandle()
