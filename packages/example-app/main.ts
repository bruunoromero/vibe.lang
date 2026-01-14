import { main } from "./dist/ExampleApp/ExampleApp";

if (typeof main === "function") {
  //@ts-ignore
  console.log(main());
} else {
  console.log(main);
}
