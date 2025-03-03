import * as fs from 'node:fs';

export const rmDir = (path) => () => {
  console.log(`Removing directory: ${path}`);
  fs.rmSync(
    path,
    { recursive: true, force: true },
  )
}

export const copyDir = ({ source, target }) => () => {
  console.log(`Copying directory ${source} to ${target}`);
  fs.cpSync(
    source,
    target,
    { recursive: true, force: true },
  )
}