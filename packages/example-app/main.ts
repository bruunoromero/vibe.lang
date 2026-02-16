import { main } from "./dist/ExampleApp/ExampleApp";

if (typeof main === "function") {
  console.log(main());
} else {
  console.log(main);
}
